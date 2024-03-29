#' @title Predict MBG (INLA) draws
#'
#' @description Generate draws from a fitted INLA MBG model across a
#'   space-time modeling domain. Draws are generated at the same
#'   resolution as simple_raster (probably 5x5km).
#'
#' @param res_fit A fitted INLA model object
#'
#' @param cs_df A dataframe containing mean and standard deviations used to scale the covariates
#'
#' @param mesh_s An 'inla.mesh' object used to fit the spatial SPDE GP approximation in res_fit
#'
#' @param mesh_t An 'inla.mesh' object used to fit the temporal correlation structure in res_fit
#'
#' @param cov_list A named list containing covariate rasters used in the model fit
#'
#' @param samples Integer number of draws to take from the fitted posterior distribution
#'
#' @param simple_raster A simple_raster defining the modeling domain pixels
#'
#' @param transform String name of transformation to be applied to
#'   draws. Possible options are 'inverse-logit'. Any other string
#'   will result in the identity transform.
#'
#' @param ind indicator string 
#'
#' @param indg indicator_group string
#'
#' @param rd run_date string
#'
#' @param region region string
#'
#' @param full_df data.frame/table containing observational data used to fit model
#'
#' @param coefs.sum1 Logical. Were the coefficients constrained to sum-to-1 in the model fit
#'
#' @param pred_gp Logical. Should gaussian process draws be added to the draw objects.
#'
#' @param fixed_effects string containing fixed effects used in res_fit separated by " + "
#'
#' @param yl numeric vector of years in model
#'
#' @param nperiod number of temporal periods in res_fit
#'
#' @param stacker_names string vector containing names of child models used for stacking in res_fit
#'
#' @param shapefile_version Shapefile version
#' 
#' @param simple_raster_subnats subnational simple raster used for subnational REs
#' 
#' @param subnat_country_to_get Vector of iso3 codes to get subnational REs
#'
#' @param no_nugget_predict logical indicating if the nugget should be used in prediction.
#'
#' @param use_space_only_gp Logical. include a space only (time stationary) gp
#' 
#' @param use_time_only_gmrf Logical. include a time only gp
#' 
#' @param use_timebyctry_res Logical. include a time only gp by country
#'
#' @importFrom raster mask crop extent setExtent xyFromCell resample
#' @importFrom Matrix crossprod t
#' @rdname predict_mbg
#' 
#' @return list containing mean raster, sd raster, and a matrix of
#'   space-time pixel cell predictions. The cell prediction matrix is
#'   wide on draws and long on space-time locations. The space-time
#'   rows are formatted such that all pixels in the first period take
#'   up rows 1:num.space.pixels, Then period 2 space pixels are next
#'   and so on.
#' 
#' @export
predict_mbg <- function(res_fit, cs_df, mesh_s, mesh_t, cov_list,
                        samples, simple_raster, transform,
                        ind=indicator, indg=indicator_group,
                        rd=run_date, region = reg, full_df = df,
                        coefs.sum1 = FALSE, pred_gp = TRUE,
                        fixed_effects = all_fixed_effects, yl = year_list,
                        nperiod = ifelse(is.character(year_list),
                                         length(eval(parse(text = year_list))),
                                         length(year_list)),
                        stacker_names = child_model_names,
                        shapefile_version = 'current',
                        simple_raster_subnats = NULL,
                        subnat_country_to_get = NULL,
                        no_nugget_predict = use_global_if_missing("no_nugget_predict"),
                        use_space_only_gp = FALSE,
                        use_time_only_gmrf = FALSE,
                        use_timebyctry_res = FALSE,
                        seed = NULL
) {
  
  if(nchar(stacker_names[1]) == 0 & coefs.sum1 == TRUE){
    message("WARNING! You've chosen sum-to-1 but don't appear to be using any stackers. Unless you have a very good reason to do this, it probably doesn't make sense. As such, we're setting coefs.sum1 <- FALSE")
    coefs.sum1 <- FALSE
  }
  
  ## Now that we've extracted covariate values to our data in the buffer zone, clip cov list to simple_raster instead of simple_polygon
  ##    (simple_raster is area we actually want to model over)
  for(l in 1:length(cov_list)) {
    message(sprintf("On cov %i out of %i", l, length(cov_list)))
    ## res(cov_list[[l]]) <- res(simple_raster) ## to fix strange issue with PERU covs having different x-resolution
    cov_list[[l]]  <- raster::crop(cov_list[[l]], raster::extent(simple_raster))
    cov_list[[l]]  <- raster::setExtent(cov_list[[l]], simple_raster)
    cov_list[[l]]  <- raster::mask(cov_list[[l]], simple_raster)
  }
  
  #updated by DCCASEY on 11-21-2016
  
  
  message('Making predictions')
  
  ## number of samples
  n_draws <- samples
  
  ## dummy raster
  template <- simple_raster
  
  cell_idx <- seegSDM:::notMissingIdx(template)
  
  ## sample from posterior over latents
  if(!is.null(seed)) {
    set.seed(seed)
    suppressWarnings(draws <- inla.posterior.sample(n_draws, res_fit, seed = seed))
  } else {
    suppressWarnings(draws <- inla.posterior.sample(n_draws, res_fit))
  }
  seed <- increment_seed(seed)

  ## Save full inla draws object above to get hyperpar draws later (tau for Gaussian, etc.)
  run_dir <- paste0('/share/geospatial/mbg/', indg, '/', ind, '/output/', rd, '/inla_draws')
  dir.create(run_dir, showWarnings = FALSE)
  save(list='draws', file = paste0(run_dir, '/inla_draws_', region, '.RData'))
  
  ## get parameter names
  par_names <- rownames(draws[[1]]$latent)
  
  ## index to spatial-temporal field and linear coefficient samples
  s_idx <- grep('^space.*', par_names) ## space-time random effects
  
  ## index for spatial field
  s_no_t_idx <- grep('^sp.no.t.*', par_names) ## space random effects
  
  if(coefs.sum1){
    l_idx <- grep('covar', par_names)
    l_idx_int <- match(sprintf('%s.1', res_fit$names.fixed), ## main effects
                       par_names)
    if(mean(is.na(l_idx_int)) == 1) l_idx_int <- match(sprintf('%s', res_fit$names.fixed), par_names)
    if(mean(is.na(l_idx_int)) == 1) l_idx_int <- match(sprintf('%s:1', res_fit$names.fixed), par_names)##
    l_idx = c(l_idx, ## covars in sum-to-1
              l_idx_int) ## intercept
    
    ## check
    if(length(l_idx) < 1){ ## 1 b/c intercept still will show up
      stop('Looks like you may have requested sum.to.1 in predict_mbg on a model that was not run using sum.to.1 constraint.')
    }
    
  }else{
    l_idx <- match(sprintf('%s.1', res_fit$names.fixed), ## main effects
                   par_names)
    if(mean(is.na(l_idx)) == 1) l_idx <- match(sprintf('%s', res_fit$names.fixed), par_names) ## fix for different INLA version naming conventions
    if(mean(is.na(l_idx)) == 1) l_idx <- match(sprintf('%s:1', res_fit$names.fixed), par_names)##
    
    ## check
    if(length(l_idx) < 1){ ## 1 b/c of the intercept
      stop('Looks like you may have set coefs.sum.1 to FALSE in predict_mbg using a fitted model that WAS run using sum.to.1 constraint')
    }
  }
  
  ## get samples of space-time REs as matrices
  if(pred_gp){
    pred_s <- sapply(draws, function(x) {
      x$latent[s_idx]})
  }
  
  ## get samples of space REs as matrices
  if(use_space_only_gp){
    pred_s_no_t <- sapply(draws, function (x)
      x$latent[s_no_t_idx])
  }
  
  ## get samples of other effects/params as matrices
  pred_l <- sapply(draws, function(x) {
    x$latent[l_idx]})
  if(length(l_idx)==1) pred_l=t(as.matrix(pred_l))
  
  if(coefs.sum1){
    rownames(pred_l) <- c(stacker_names, ## covars in sum-to-1
                          res_fit$names.fixed) ## intercept
  } else {
    rownames(pred_l) <- res_fit$names.fixed
  }
  
  ## if we fit with a nugget and include it in prediction, we also need to take draws of the nugget precision
  if(length(grep("^IID.ID.*", par_names)) > 0 & no_nugget_predict == FALSE){
    pred_n <- sapply(draws, function(x) {
      nug.idx <- which(grepl('IID.ID', names(draws[[1]]$hyper)))
      x$hyperpar[[nug.idx]]}) ## this gets the precision for the nugget
  }else{
    pred_n <- NULL
  }
  
  ## check to see if country random effects were used
  if(sum(grepl('CTRY.ID', par_names)) > 0){
    
    ## then country random effects were used - get the fitted values
    ctry.res.idx <- grep('CTRY.ID*', par_names)
    pred_ctry_res  <-  sapply(draws, function (x) x$latent[ctry.res.idx])
    ctry.res.names <- par_names[ctry.res.idx]
    if (length(ctry.res.names) == 1) {
      message('WARNING:ONLY ONE COUNTRY PRESENT IN DATA AND RE FIT')
      print('WARNING:ONLY ONE COUNTRY PRESENT IN DATA AND RE FIT')
      pred_ctry_res <- matrix(pred_ctry_res, nrow = 1)
    }
    rownames(pred_ctry_res) <- res_fit$summary.random$CTRY.ID$ID
    
    ## also get the hyperparam precision in case not all countries had data and we need to take draws
    pred_ctry_prec <- sapply(draws, function(x){
      ctry.prec.idx <- which(grepl('CTRY.ID', names(draws[[1]]$hyper)))
      x$hyperpar[[ctry.prec.idx]]}) ## this gets the precision for country REs
  }else{
    pred_ctry_res <- NULL
  }
  
  if(use_time_only_gmrf){
    ## then main temporal effects were used - get the fitted values
    
    if(use_timebyctry_res) {
      timebyctry.res.idx <- grep('ADM0.ID*', par_names)
      pred_time_res  <-  sapply(draws, function (x) x$latent[timebyctry.res.idx])
      timebyctry.res.names <- par_names[timebyctry.res.idx]
      rownames(pred_time_res) <- timebyctry.res.names
    } else {
      t_no_s_idx <- grep('^t.no.sp.*', par_names) ## time random effects
      pred_time_res  <-  sapply(draws, function (x) x$latent[t_no_s_idx])
      time.res.names <- par_names[t_no_s_idx]
      if (length(time.res.names) == 1) {
        message('WARNING:ONLY ONE YEAR PRESENT IN DATA AND RE FIT')
        print('WARNING:ONLY ONE YEAR PRESENT IN DATA AND RE FIT')
        pred_time_res <- matrix(pred_time_res, nrow = 1)
      }
      rownames(pred_time_res) <- res_fit$summary.random$t.no.sp$ID
    }
    
  }else{
    pred_time_res <- NULL
  }
  
  ## check to see if admin-1 (subnat) random effects were used
  if (sum(grepl("SUBNAT.ID", par_names)) > 0) {
    
    num_countries <- sum(grepl("SUBNAT.ID*", names(res_fit$summary.random)))
    subnat.res.idx <- grep("SUBNAT.ID", par_names)
    pred_subnat_res <- sapply(draws, function(x) x$latent[subnat.res.idx])
    subnat.res.names <- par_names[subnat.res.idx]
    if (length(subnat.res.names) == 1) {
      message("WARNING:ONLY ONE SUBNAT UNIT PRESENT IN DATA AND RE FIT")
      print("WARNING:ONLY ONE SUBNAT UNIT PRESENT IN DATA AND RE FIT")
      pred_subnat_res <- matrix(pred_subnat_res, nrow = 1)
    }
    
    #get names from multiple SUBNAT.ID tables
    subnat_with_data <- vector("list", length = num_countries)
    for(i in 1:num_countries) {
      temp <- res_fit$summary.random[[paste0("SUBNAT.ID", i)]]
      subnat_with_data[[i]] <- temp$ID
    }
    rownames(pred_subnat_res) <- unlist(subnat_with_data)
    
    pred_subnat_prec <- vector("list", length = num_countries)
    for(i in 1:num_countries) {
      ## also get the hyperparam precision in case not all countries had data and we need to take draws
      pred_subnat_prec[[i]] <- sapply(draws, function(x) {
        subnat.prec.idx <- which(grepl(paste0("SUBNAT.ID",i), names(draws[[1]]$hyper)))
        x$hyperpar[[subnat.prec.idx]]
      }) ## this gets the precision for country REs
    }
    
    #use the first subnat code of each country to get all country codes with subnats, then pull all subnat codes for those countries
    #can use %% 1000 to get nat codes from subnats due to structure of codes, e.g. subnat 24032 is in nat 32 and subnat 24145 is in nat 145
    all_subnats <- vector("list", length = num_countries)
    for(i in 1:num_countries){
      all_subnats[[i]] <- get_adm_codes_subnat(subnat_with_data[[i]][1] %% 1000, admin_level = 1, shapefile_version = modeling_shapefile_version)
    }
  } else {
    pred_subnat_res <- NULL
  }
  
  ## get coordinates of cells to predict to
  ## these are are needed both for raster extraction and GP projection
  coords <- raster::xyFromCell(template, seegSDM:::notMissingIdx(template))
  
  ## Create coords and stuff for the subnational simple raster
  if(!is.null(simple_raster_subnats)) {
    template_subnat <- simple_raster_subnats
    subnat_cell_idx <- seegSDM:::notMissingIdx(template_subnat)
    subnat_coords <- raster::xyFromCell(template_subnat, seegSDM:::notMissingIdx(template_subnat))
  }
  
  
  
  ## setup coords for GP projection. convert coords to spherical if
  ## using spherical modeling mesh. used if you made a mesh on s2
  if(mesh_s$manifold == "S2"){
    
    gp_coords <- lonlat3D(coords[, 1], coords[, 2])
    
  } else {
    gp_coords <- coords
  }
  
  # ~~~~~~~~~~~~~~
  # project spatial effect
  
  # make predictions for all periods
  
  # get predictor matrix between spatial nodes and prediction locations
  #nperiod <- mesh_t$n
  #nperiod <- max(mesh_t$loc)
  ## We now take this as a argument based on the length of the year_list vector
  
  ## replicate coordinates and years for GP projection
  gp_coords_periods <- do.call(rbind,
                               replicate(nperiod,
                                         gp_coords,
                                         simplify = FALSE))
  
  groups_periods <- rep(1:nperiod,
                        each = nrow(gp_coords))
  
  ## Projector matrix for space-time effects
  A.pred <- inla.spde.make.A(mesh = mesh_s,
                             loc = gp_coords_periods,
                             group = groups_periods,
                             group.mesh = mesh_t)
  
  A.pred.s.no.t <- inla.spde.make.A(mesh = mesh_s,
                                    loc = gp_coords)
  
  ## get samples of s-t effect for all cells and across time
  if(pred_gp){
    # Using `crossprod()` here for product of a large, sparse matrix and a dense matrix is ~2x
    # faster than using `%*%`
    # cell_s <- as.matrix(crossprod(t(A.pred), pred_s))
    cell_s <- as.matrix(Matrix::crossprod(Matrix::t(A.pred), pred_s))
  }else{
    cell_s <- 0
  }
  
  ## get samples of the spatial effect at all cells and replicate across time
  if(use_space_only_gp){
    ## get the stationary spatial effect
    cell_s_no_t <- as.matrix(Matrix::crossprod(Matrix::t(A.pred.s.no.t), pred_s_no_t))
    ## and replicate across time
    cell_s_no_t <- do.call('rbind', rep(list(cell_s_no_t), nperiod))
  }else{
    cell_s_no_t <- 0
  }
  
  # remove out temporally varying covariates for now, will deal with them later
  #split the effects names by varying or time varying
  tvnames = pars =c()
  for(i in 1:length(cov_list)){
    if(dim(cov_list[[i]])[3]!=1) tvnames[length(tvnames)+1]= names(cov_list)[i]
    if(dim(cov_list[[i]])[3]==1) pars[length(pars)+1]= names(cov_list)[i]
  }
  
  #now split the covariates themselves (not just the names)
  # process covariates
  
  ## start by ensuring the same mask and cell size
  ## I think we don't need this next line anymore since everything should be done at the top of this function...
  cov_list = setNames(lapply(cov_list, function(x) {
    raster::mask(raster::resample(x, template), template)
  }), names(cov_list))
  
  #extract the covariates
  vals = data.table(do.call(cbind, lapply(cov_list, function(x) raster::extract(x,coords))))
  
  #create the int column
  vals[,int:= 1]
  
  #reshape long
  vals[,id:= 1:nrow(vals)] #create an id to ensure that melt doesn't sort things
  
  if (!is.null(mesh_t)){
    #convert the time varying names into meltable values
    tv_cov_colnames = grep(paste(tvnames, collapse = "|"), names(vals), value = T) #unlisted
    tv_cov_colist = lapply(tvnames, function(x) grep(paste0("^", x, "\\.[[:digit:]]*$"), names(vals), value = T))
    
    vals = melt(vals, id.vars = c('id','int',pars), measure = tv_cov_colist, value.name = tvnames, variable.factor =F)
    
    #fix the names
    #melt returns different values of variable based on if its reshaping 1 or 2+ columns.
    #enforce that it must end with the numeric after the period
    vals[,variable:= as.numeric(substring(variable,regexpr("(\\.[0-9]+)$", variable)[1]+1))]
  }
  
  #Tests suggest this is not needed anymore/as a carry over. Keep just in case
  #if(length(tv_cov_colist)==1){
  #  setnames(vals,paste0(tvnames[[1]],'1'), tvnames)
  #}
  
  #keep only columns that were part of the regression
  vals = vals[,mget(rownames(pred_l))]
  
  #centreScale the values
  vals = centreScale(vals,cs_df) #happens for all periods at once
  
  #create the grid of fixed effects
  # Conversion to the "dgeMatrix" here will gives better performance for matrix
  # multiplication of two dense matrices. Conversion back to standard unnamed
  # 'matrix' object slows the operation down a bit, but ensures that the later
  # `apply` operations will be as fast as possible. Still much faster this way
  # and much faster when using the MKL.
  cell_l <- unname(as.matrix(as(data.matrix(vals), "dgeMatrix") %*% pred_l))
  
  ## add on nugget effect if applicable
  if(!is.null(pred_n)){
    set.seed(seed)
    seed <- increment_seed(seed)
    cell_n <- sapply(pred_n, function(x){
      rnorm(n = nrow(cell_l), sd = 1 / sqrt(x), mean = 0)
    })
  }
  
  
  ## add on country random effects if applicable
  if(!is.null(pred_ctry_res)){
    
    cell_ctry_res <- matrix(0, ncol = ncol(cell_l), nrow = nrow(cell_l))
    
    ## loop through the countries in the region and add on random
    ## effects if we have a fitted value. otherwise add on iid values
    ## drawn with the precision for the random effects hyperparam
    reg.gauls <- get_adm0_codes(reg, shapefile_version = shapefile_version)
    gauls.with.res <- as.numeric(rownames(pred_ctry_res))
    gaul.vec <- values(simple_raster)[cell_idx]
    gaul.t.vec <- rep(gaul.vec, nperiod) ## expand gaul vec in time
    
    set.seed(seed)
    seed <- increment_seed(seed)

    for(gg in reg.gauls){
      ## add on random effect to those pixels belonging to the country
      gg.rows <- which(gaul.t.vec == gg)
      
      if(gg %in% gauls.with.res){
        message(paste0('adding RE for country ', gg))
        gg.idx  <- which(gauls.with.res == gg)
        cell_ctry_res[gg.rows, ] <- cell_ctry_res[gg.rows, ] +
          matrix(rep(pred_ctry_res[gg.idx, ], length(gg.rows)), ncol = ncol(cell_ctry_res), byrow = TRUE)
      }else{
        ## this country had no data and we need to take a draw from the RE dist for draw in preds
        ## for draw, we draw one value of the random effect using pred_ctry_prec
        re.draws <- rnorm(n = ncol(cell_ctry_res), mean = 0, sd = 1 / sqrt(pred_ctry_prec))
        ## replicate by all rows in that country and add it on
        cell_ctry_res[gg.rows, ] <- cell_ctry_res[gg.rows, ] +
          matrix(rep(re.draws, length(gg.rows)), ncol = ncol(cell_ctry_res), byrow = TRUE)
      }
      
    }
    
  }
  
  # The final cell object should be arranged by space then time such that all
  # years are clustered together and in ascending order. 
  if(!is.null(pred_time_res)){
    cell_time_res <- matrix(0, ncol = ncol(cell_l), nrow = nrow(cell_l))

    if(use_timebyctry_res) {
      reg.gauls <- get_adm0_codes(reg, shapefile_version = shapefile_version)
      gaul.vec <- values(simple_raster)[cell_idx]
      gaul.t.vec <- rep(gaul.vec, nperiod) ## expand gaul vec in time
      for(gg in reg.gauls){
        message(paste0('adding Time RE for country ', gg))
        gg.idx.cell  <- which(gaul.t.vec == gg)
        gg.idx.pred <- grep(paste0("ADM0.ID", gg,":*"),rownames(pred_time_res))
        cell_time_res[gg.idx.cell,] <- cell_time_res[gg.idx.cell,] + matrix(rep(pred_time_res[gg.idx.pred, ], each=sum(gaul.vec == gg)), ncol = ncol(cell_time_res))
      }
    } else {
      gaul.vec.N <- length(values(simple_raster)[cell_idx])
      idx_hold <- 0 
      for(y_ in sort(yl)){
        tidx <- ((idx_hold*gaul.vec.N)+1):((idx_hold+1)*gaul.vec.N)
        cell_time_res[tidx,] <- sapply(
          pred_time_res[as.character(y_),], 
          function(it) rep(it, gaul.vec.N))
        idx_hold <- idx_hold + 1
      }
    }
    
  }
  
  ## add on subnational random effects if applicable
  if (!is.null(pred_subnat_res) & !is.null(subnat_country_to_get)) {
    cell_subnat_res <- matrix(0, ncol = ncol(cell_l), nrow = nrow(cell_l))
    
    #get vector of subnat adm codes matching cell_subnat_res
    subnat_gauls.with.res <- as.numeric(rownames(pred_subnat_res))
    subnat_gaul.vec <- values(simple_raster2)[cell_idx]
    subnat_gaul.t.vec <- rep(subnat_gaul.vec, nperiod)
    
    set.seed(seed)
    seed <- increment_seed(seed)
    
    #for all subnat adm codes in countries specified for subnational res
    #if the code is in subnat_with_data, add to cell_subnat_res from posterior
    #otherwise draw from prior
    #countries not specified in subnat_country_to_get will be 0 in cell_subnat_res
    for(i in 1:num_countries){
      for(gsubnat in all_subnats[[i]]) {
        
        ## Find the rows in simple_raster2 corresponding to the iterating subnat
        gsubnat.rows <- which(subnat_gaul.t.vec == gsubnat)
        
        if (gsubnat %in% subnat_with_data[[i]]) {
          
          gg.idx <- which(subnat_gauls.with.res == gsubnat)
          
          message(paste0("adding RE for subnat ", gsubnat))
          
          cell_subnat_res[gsubnat.rows, ] <- cell_subnat_res[gsubnat.rows, ] + 
            matrix(rep(pred_subnat_res[gg.idx, ], length(gsubnat.rows)), ncol = ncol(cell_subnat_res), byrow = TRUE)
          
        } else {
          
          message(paste0("Adding RE for subnat ", gsubnat, " - No data found, pulling from prior"))
          ## this subnat unit had no data and we need to take a draw from the RE dist for draw in preds
          ## for draw, we draw one value of the random effect using pred_subnat_prec
          re.draws <- rnorm(n = ncol(cell_subnat_res), mean = 0, sd = 1 / sqrt(pred_subnat_prec[[i]]))
          
          ## replicate by all rows in that country and add it on
          cell_subnat_res[gsubnat.rows, ] <- cell_subnat_res[gsubnat.rows, ] + 
            matrix(rep(re.draws, length(gsubnat.rows)), ncol = ncol(cell_subnat_res), byrow = TRUE)
        }
      }
    }
  }
  
  ## get sd from fixed effects
  cell_l_sd <-  apply(cell_l, 1, sd)
  
  ## project model uncertainty from node-level sd to pred grid level for s-t REs
  if(pred_gp){
    node_s_sd <- apply(pred_s, 1, sd)
    # Large, sparse matrix-vector product faster as `%*%` than with `crossprod()`
    cell_s_sd <- as.matrix(A.pred %*% node_s_sd)
  }else{
    cell_s_sd <- 0
  }
  
  ## project model uncertainty from node-level sd to pred grid level for spatial REs
  if(use_space_only_gp){
    node_s_no_t_sd <- apply(pred_s_no_t, 1, sd)
    ## Large, sparse matrix-vector product faster as `%*%` than with `crossprod()`
    cell_s_no_t_sd <- as.matrix(A.pred.s.no.t %*% node_s_no_t_sd)
    ## replicate in time
    cell_s_no_t_sd <- rep(cell_s_no_t_sd, nperiod)
  }else{
    cell_s_no_t_sd <- 0
  }
  
  ## get uncertainty from nugget
  if(!is.null(pred_n)){
    cell_n_sd <- apply(cell_n, 1, sd)
  }
  
  ## get uncertainty from country res
  if(!is.null(pred_ctry_res)){
    cell_ctry_res_sd <- apply(cell_ctry_res, 1, sd)
  }
  
  ## get uncertainty from subnat res
  if (!is.null(pred_subnat_res)) {
    cell_subnat_res_sd <- apply(cell_subnat_res, 1, sd)
  }
  
  ## get uncertainty from time only effects
  if(!is.null(pred_time_res)){
    cell_time_res_sd <- apply(cell_time_res, 1, sd)
  }
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## combine to get cell-level sd
  ## ASSUMES INDEPENDENCE!
  
  # do all of this by adding squares
  cell_sd <- cell_l_sd ^ 2 + cell_s_sd ^ 2 + cell_s_no_t_sd ^ 2
  
  # add nugget if present
  if (!is.null(pred_n)) {
    cell_sd <- cell_sd + cell_n_sd^2
  }
  
  # add country res if present
  if (!is.null(pred_ctry_res)) {
    cell_sd <- cell_sd + cell_ctry_res_sd^2
  }
  
  # add subnat res if present
  if (!is.null(pred_subnat_res)) {
    cell_sd <- cell_sd + cell_subnat_res_sd^2
  }
  
  # add time independent if present
  if(!is.null(pred_time_res)){
    cell_sd <- cell_sd + cell_time_res_sd^2 
  }
  
  # finally, take the square root
  cell_sd <- sqrt(cell_sd)
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # combine to produce draws (untransformed)
  cell_all <- cell_l + cell_s + cell_s_no_t
  
  # add nugget if present
  if (!is.null(pred_n)) {
    cell_all <- cell_all + cell_n
  }
  
  # add country res if present
  if (!is.null(pred_ctry_res)) {
    cell_all <- cell_all + cell_ctry_res
  }
  
  # add subnat res if present
  if (!is.null(pred_subnat_res)) {
    cell_all <- cell_all + cell_subnat_res
  }
  
  # add time independent if present
  if(!is.null(pred_time_res)){
    cell_all <- cell_all + cell_time_res 
  }
  
  # get predictive draws on probability scale
  if(transform == 'inverse-logit') {
    cell_pred <- plogis(as.matrix(cell_all))
  } else {
    cell_pred <- eval(parse(text = transform))
  }
  
  # get prediction mean (integrated probability)
  pred_mean <- rowMeans(cell_pred)
  
  # create multi-band rasters for each of these metrics
  # each band giving the values for a different period
  mean_ras <- insertRaster(template,
                           matrix(pred_mean,
                                  ncol = nperiod))
  
  sd_ras <- insertRaster(template,
                         matrix(cell_sd,
                                ncol = nperiod))
  
  names(mean_ras) <- paste0('period_', 1:nperiod)
  names(sd_ras)   <- paste0('period_', 1:nperiod)
  
  return_list <- list(mean_ras, sd_ras, cell_pred)
  
  return(return_list)
  
}