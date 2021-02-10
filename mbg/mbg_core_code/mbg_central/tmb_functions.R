


#' @title Build Stack for TMB MBG model
#' @description Organize Data and Parameter Stacks to fit a TMB MBG model
#' @author Roy Burstein
#'
#' @param d prepped model frame with no NAs
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of zcol for analysis (i.e. ages c(1,2,3,4,5). Must be integers starting with 1)
#' @param fes a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#'  corresponding to column names in d
#' @param indic indicator name, corresponding to the response variable in d
#' @param country_re TRUE/FALSE include country re. If true and there is a zcol then there will be random slope as well (todo)
#' @param nid_re TRUE/FALSE include re on survey. Need nid column in the data frame. defaults to use_nid_res from config. 
#' @param exclude_cs character vector of covariates to exclude from centrescaling
#' @param mesh_s an inla mesh object 
#' @param spde_prior String containing a list. Specifies the type of prior used
#'   for the Matern model parameters. If type = "nonpc" then use non pc 
#'   prior. User can specify nominal prior means: spde_prior$prior$range.nominal 
#'   and spde_prior$prior$variance.nominal. Defaults are INLA defaults, that is 
#'   20\% of the range of the mesh and 1, respectively. If type = "pc" then use 
#'   pc prior.  User can specify spde_prior$prior$range = (range0, Prange) i.e. 
#'   P(range < range0) = Prange and spde_prior$prior$sigma = (sigma0, Psigma) 
#'   i.e. P(sigma > sigma0) = Psigma. Defaults for Prange and Psigma are 0.05. 
#'   Default for range0 is 5\% of the range of the mesh and sigma0=3.
#' @param nugget Logical. If TRUE, include a nugget effect
#' @param ycol column name of y values, defaults to year
#' @param zcol column name of z values associated with zl.
#' @param shapefile_version character. Version of shape file to use.
#' @param scale_gaussian_variance_N Logical. Do you want to scale gaussian variance by sample size?
#' @param cov_constraints named int vector. integer vector indexed by covariate 
#'   names, in the format returned by the function 
#'   \code{\link{covariate_constraint_vectorize}}. NOTE: In the current
#'   implementation, priors for fixed effects are set to the same values
#'   regardless of whether the covariate fixed effect is constrained or not. 
#'   Apply constraints with caution.
#' @param main_space_effect TRUE/FALSE to include a space-only GP
#' @param main_time_effect TRUE/FALSE to include a time-only GMRF
#' @param main_age_effect TRUE/FALSE to include an age-only GMRF
#' @param use_full_interacting_effect TRUE/FALSE to use an interacting GP - this will be space-time if no z-dimension is present, space-age if only one yar is presented, and space-time-age if all three dimensions exist
#' @param coefs.sum1 Logical. Were the coefficients constrained to sum-to-1 in the model fit
#' @param stacker_names string vector of child model names

#'
#' @note additional config params passed through ...
#' @param indicator_family String, Data likelihood family. Only binomial and gaussian are currently available. Set in config.
#' @param nugget_prior String of R list notation containing an INLA style prior. Set in config.
#' @param ctry_re_prior String of R list notation containing an INLA style prior. Set in config.
#' @param nid_re_prior String of R list notation containing an INLA style prior. Set in config.
#' @param use_s2_mesh TRUE/FALSE If true, the SPDE FEM mesh is generated on a spherical (S^2) manifold instead of on the flat plane (R^2). Set in config.
#' @param mesh_s_max_edge String of R vector notation containing two positive reals: First number represents max triangle edge length in mesh inside modeling domain. Second number represents max triangle edge length in mesh outside modeling domain. Set in config.
#' @param mesh_s_offset String of R vector notation containing two reals: Provides an inner and an optional outer extension distance. If negative it is interpreted as a factor relative to the approximate data diameter. If positive it is the extension distance on same scale unit to the coordinates provided. Units are in lat-long degrees. Set in config.
#' @param s2_mesh_params String of R vector notation containing three positive reals: The first number defines the minimum triangle edge length allowed, the second argument defines how far the mesh should extend past the boundary, and the third argument defines the maximum allowed triangle edge length. Set in config.
#'
#' @return returns a named list with Data and Parameters to be passed into fit_mbg_tmb()
#' 
#' @section TODO: Allow users to set priors, allow different data model (gaussian to start), country random effects
#'
#' @export
build_mbg_data_stack_tmb <- function(d          = df,                 
                                     yl         = year_list,  
                                     zl         = z_list,
                                     fes        = all_fixed_effects,  
                                     indic      = indicator, 
                                     country_re = use_country_res, 
                                     nid_re     = use_nid_res,
                                     exclude_cs = '', 
                                     nugget     = FALSE,
                                     zcol       = NULL,
                                     ycol       = "year",
                                     shapefile_version = 'current', 
                                     scale_gaussian_variance_N = TRUE,
                                     mesh_s       = mesh_s,
                                     spde_prior = use_global_if_missing("spde_prior"),
                                     cov_constraints = use_global_if_missing("cov_constraints"),
                                     main_space_effect = as.logical(use_space_only_gp),
                                     main_time_effect = as.logical(use_time_only_gmrf),
                                     main_age_effect = as.logical(use_age_only_gmrf),
                                     full_interacting_effect = as.logical(use_gp),
                                     coefs.sum1 = coefs_sum1,
                                     stacker_names = use_global_if_missing("child_model_names"),
                                     ...){  
  
  # ensure d is a dt
  d <- setDT(d)
  
  # order d by row_id
  d <- d[order(row_id), ]
  
  # if working in the package, load ... args into local environment
  if(length(list(...)) > 0) {
    list2env(list(...), environment())
  }

  # fix row_id to increment by 1
  d <- unique(dplyr::select(d, row_id)) %>% dplyr::mutate(row_id_transf = 1:n()) %>%
    dplyr::right_join(d, by = "row_id")
  d <- setDT(d)
  
  # zcol
  if (!zcol %in% colnames(d)){
    message('No Z column detected')
    d[[zcol]] <- 0
  }
  if( !all(unique(d[[zcol]]) %in% zl)) {
    message('WARNING: zl and d[[zcol]] do not completely match up.. ')
    # check that we there arent values in zcol not matching z_list
    d[, dropz := !get(zcol) %in% zl]
    if(any(d$dropz != FALSE)){
      message(sprintf('WARNING: Detected some z values in zcol (%s) which were not in the z_list',zcol))
      message(sprintf('WARNING: Due to this, dropping %i rows from the input data',sum(d$dropz==TRUE)))
      print(table(d$dropz,d$age))
      d <- subset(d, dropz == FALSE)
      d[, dropz := NULL]
    }
  }
  
  # make a fake data point with no weight for the max period and Z to fill out the GP
  d <- rbind(d, d[1,])
  d[[zcol]][nrow(d)]     <- max(zl)
  d[['period']][nrow(d)] <- length(yl)
  d[['weight']][nrow(d)] <- 0
  d[['row_id_transf']][nrow(d)] <- max(d$row_id_transf) + 1
  

  # look for z dimension 
  num_z <- 1 # TODO get this from the z-list
  if(length(zl)>1){ 
    message(sprintf('More than one unique %s found, initiating Z in the GP', zcol))
    num_z <- length(zl)

    # set A proj grouping. The ordering here must match the ordering of epsilon_stz in the template
    grp <- setDT(expand.grid(1:length(yl), 1:max(zl)))
    setnames(grp,c('Var1','Var2'),c('period',zcol))
    grp[,group := 1:.N]
    d <- merge(d, grp, by = c('period',zcol), all.x = TRUE) # warning this may re-order things, so do not use d stuff from above
    
    # reorder d by row_id
    d <- d[order(row_id_transf), ]
  } else {
    # set Aproj grouping to period if there is but one z value
    d$group <- d$period
  }
  
  # coordinates at data points. these are passed to TMB in long,lat so
  # we keep them and create another set for 3d coords
  coords   <- cbind(d$longitude,d$latitude)
  
  # if we have  mesh on s2, first convert coords to spherical to project to mesh
  data.locs <- coords ## long, lat
  if(mesh_s$manifold == "S2"){
    ## then the mesh is on the sphere and we need to use 3d coords
    data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
  }
  
  
  # make a projection matrix from data to st mesh                                    
  A.proj_stz <- inla.spde.make.A(mesh  = mesh_s,
                             loc   = data.locs,
                             group = d$group)
  
  # make a projection matrix from data to s mesh
  A.proj_s <- inla.spde.make.A(
    mesh = mesh_s,
    loc = data.locs
  )
  
  A.proj_t <- d[[ycol]] - min(yl)
  A.proj_z <- d[[zcol]] - min(zl)
                                       
  # make a clean design matrix. make sure all fes appear in d
  fes  <- unlist(strsplit(fes, ' \\+ '))
  if(!all(fes %in% names(d)))  
    stop('Check your fes argument, not all covariate names appear in d.')
  if(length(fes)!=0) {
    X_xp <- as.matrix(cbind(int=1, d[,c(fes),with=FALSE]))
  } else {
    X_xp <- as.matrix(cbind(int=rep(1,nrow(d))))
  }
  
  # add in age fixed effects
  # TODO eventually do country random slopes for each FE level of Z
  if(num_z > 1){ 
    message(sprintf('Adding fixed effects for levels of zcol (%s)',zcol))
    for(z in 2:num_z){
      X_xp <- cbind(X_xp, d[[zcol]] == z)
    } 
    colnames(X_xp) <- c(colnames(X_xp)[colnames(X_xp)!=''],paste0('FE_z_level__',2:num_z))
    exclude_cs <- c(exclude_cs,paste0('FE_z_level__',2:num_z))
  }
  
  # cs_df. imports seegMBG
  cs_df <- getCentreScale(X_xp, exclude = c('int',exclude_cs))
  X_xp  <- centreScale(X_xp, df = cs_df)
  
  # get data range in case we want to clamp for prediction later
  clamper <- data.table(apply(X_xp,2,range))
  
  # create stacker_col_id to identify which columns in stacker have sum-to-1
  if(coefs.sum1 == TRUE) {
    stacker_col_id <- ifelse(colnames(X_xp) %in% stacker_names, 1, 0)
  } else {
    stacker_col_id <- rep(0, ncol(X_xp))
  }
  
  # sort nugget and RE indicators
  nugget     <- as.numeric(as.logical(nugget))
  country_re <- as.numeric(as.logical(country_re))
  nid_re     <- as.numeric(as.logical(nid_re)) # these do not get used in prediction
  
  # obtain d_i (unique observations)
  d_i <- d[first_entry==1, ]
  
  # check there is more than one observed country or nid if REs for those are set
  if(length(unique(d_i$country)) == 1 & country_re == TRUE){
    message('WARNING: Only found one unique country in this data frame, so turning off country random effects.')
    country_re <- FALSE
  }
  if(length(unique(d_i$nid)) == 1 & nid_re == TRUE){
    message('WARNING: Only found one unique NID in this data frame, so turning off NID random effects.')
    nid_re <- FALSE
  }
  
  # make a table mappind the Admin0 code to a unique country random effect
  md    <- get_location_code_mapping(shapefile_version = shapefile_version)
  mdsub <- md[ihme_lc_id %in% unique(as.character(d_i$country)),]
  if(nrow(mdsub) != length(unique(as.character(d$country)))){
    message(sprintf('get_location_code_mapping() COUNTRY NAMES: %s',paste(sort(mdsub$ihme_lc_id),collapse=', ')))
    message(sprintf('IN DATA COUNTRY NAMES: %s',paste(sort(unique(as.character(d$country))),collapse=', ')))
    stop('get_location_code_mapping() and countries in data not of matching lengths')
  }
  cntry_re_map <- data.table(
                    country   = mdsub$ihme_lc_id,
                    adm_code = mdsub$ADM_CODE,
                    re_id     = 0:(nrow(mdsub)-1))
  cntry_re_vec <- cntry_re_map$re_id[match(as.character(d_i$country),cntry_re_map$country)]
 
  # make an nid_re mapping table
  nid_re_map <- unique(dplyr::select(d_i, nid)) %>%
    dplyr::mutate(re_id = 0:(n()-1))
  nidEFF <- dplyr::select(d_i, nid) %>% dplyr::left_join(nid_re_map, by="nid")
  nid_re_vec <- nidEFF$re_id
  countries_with1nid <- dplyr::select(d_i, country, nid) %>%
     dplyr::group_by(country) %>% # group by country to see...
     dplyr::mutate(nidCount=n()) %>% # the number of nid units per country
     dplyr::filter(nidCount==1) %>% dplyr::ungroup %>% dplyr::select(country) %>% unique
  if(country_re==TRUE & nid_re==TRUE & nrow(countries_with1nid) > 0) {
    message(paste("WARNING! The following countries", countries_with1nid$country, 
                   "have only 1 NID. If you encounter convergence problems, you 
                  may want to remove NID random effects."))
  }
  
  # Pixel re
  pixelEFF <- unique(dplyr::select(d, pixel_id)) %>%
    dplyr::mutate(pixel_id_transf = 1:n()) %>%
    dplyr::right_join(d, by = "pixel_id")
  
  pixel_re_vec <- pixelEFF$pixel_id_transf-1
  
  # set GP RE array, or matrix depending on if we have a z dimension
  if(num_z > 1) {
    Epsilon_stz <- array(0, dim=c(mesh_s$n,length(yl),num_z))
  } else {
    Epsilon_stz <- array(0, dim=c(mesh_s$n,length(yl)))
  }
  
  Epsilon_s <- rep(0, mesh_s$n)
  Epsilon_t <- rep(0, length(yl))
  Epsilon_z <- rep(0, num_z)
  
  
  # set up vectors of model family 
  # look for convention in the data of lik_fam_<<binom,gauss>>, if not there, default to config family
  lik_gaussian <- lik_binomial <- rep(0, nrow(d_i))
  if(('lik_fam_binom' %in% names(d_i)) & ('lik_fam_gauss' %in% names(d_i))) {
    lik_gaussian <- d_i$lik_fam_gauss
    lik_binomial <- d_i$lik_fam_binom
    message(sprintf('Found row specific data likelihood indicators, will use those. %i rows binom, %i rows gauss',
                    sum(lik_binomial),sum(lik_gaussian)))
  } else {
    if(indicator_family == 'binomial') {
      lik_binomial <- rep(1, nrow(d_i))
      message('Using indicator family binomial for all rows')
    } else if(indicator_family == 'gaussian') {
      lik_gaussian <- rep(1, nrow(d_i))
      message('Using indicator family gaussian for all rows')
    }
  }

  if(any(lik_gaussian+lik_binomial != 1))
    stop('Not all rows in your data have been assigned a model (binom or gauss), or some have been assigned multiple!')
  
  # also look for sd if already exists in the data for the gauss rows to use
  # This is useful for crosswalked values with some data uncertainty, convention is variable named sd_<<INDICATOR>>
  sd_i <- rep(0, nrow(d_i))
  if(paste0('sd_',indicator) %in%  names(d)){
    message('Found SD estimates to use for crosswalked values.')
    sd_i <- d_i[[paste0('sd_',indicator)]]
    sd_i[is.na(sd_i)] <- 0
  }


  # run a quick regression to get starting values for the fixed effects 
  # This can speed up model fitting if iterations are slow.
  # Note this fit is currently assuming binomial
  # Note sometimes the starting values can strongly impact results
  if(all(lik_binomial==1) & coefs.sum1 != TRUE){
    message('LM for starting fe vals')
    y <- (d_i[[indic]][lik_binomial==1]+.0001)/d_i$N[lik_binomial==1]
    y[y<=0] <- 0.001
    y[y>=1] <- 0.999
    fe_start <- round( unname( lm(qlogis(y) ~ -1 + X_xp[d$first_entry==1 & lik_binomial==1,])$coefficients ), 4)
  } else {
    message('Default starting fe vals')
    fe_start <- rep(0,ncol(X_xp))
  }
  message(sprintf('starting values for fixed effects: %s',paste0(fe_start,collapse=', ')))
  
  
  # cannot allow a gaussian likelihood to also have a nugget in the linear term, it leads to issues
  if(nugget == 1 & all(lik_gaussian == 1)){
    message('WARNING:: Nugget in all gaussian model leads to identifiability issues. Removing nugget for you.')
    nugget <- 0
  }
  
  
  # check if user wants to scale gaussian variance by N, if not set them all to one in the gaussian rows
  # TODO is this still true when it is a pixel re?
  n_i <- d_i$N
  if(scale_gaussian_variance_N == FALSE & sum(lik_gaussian)>1) {
    message('Not scaling gaussian error by N since scale_gaussian_variance_N == FALSE.')
    n_i[lik_gaussian==1] <- 1
  }

  # if there is only one country in the region, turn off country_re
  if(all(country_re == TRUE & length(get_adm0_codes(reg)) == 1)){
    message('WARNING: One country in this region, so turning off country random effects')
    country_re <- FALSE
  }
  
  # print some messages for random effects
  if(nugget == TRUE)     message('USING PIXEL RANDOM EFFECTS')
  if(country_re == TRUE) message('USING COUNTRY RANDOM EFFECTS')
  if(nid_re == TRUE)     message('USING NID RANDOM EFFECTS')
  
  
  # Build SPDE object (using INLA functions) and get prior in TMB readable format
  spde_list <- read_inla_prior_matern(spde_prior, mesh_s)
  spde <- spde_list$spde
  
  # Construct a list of all data necessary to TMB to fit
  Data <- list(
    num_z = num_z,
    y_i = d_i[[indic]],      # Number of observed events in the cluster (N+ in binomial likelihood)
    n_i = d_i$N,             # Number of observed exposures in the cluster (N in binomial likelihood)
    c_re_i = cntry_re_vec, # vector of country ids, ( starting at zero because C)
    nid_re_i = nid_re_vec, # vector of survey ids, zero index added in cpp for null effect
    pixel_re_k = pixel_re_vec, # vector of pixel ids
    w_i = d_i$weight,        # Data weight for each row
    X_kj = X_xp,           # Covariate design matrix
    M0 = spde$param.inla$M0, # SPDE sparse matrix
    M1 = spde$param.inla$M1, # SPDE sparse matrix
    M2 = spde$param.inla$M2, # SPDE sparse matrix
    Aproj_stz = A.proj_stz, # mesh to prediction point projection matrix for stz
    Aproj_s = A.proj_s,   # mesh to prediction point projection matrix for s
    Aproj_t = A.proj_t,   # mesh to prediction point projection matrix for t
    Aproj_z = A.proj_z,   # mesh to prediction point projection matrix for z
    ID_k = d$row_id_trans-1,       # observation ID
    lik_gaussian_i = lik_gaussian, # data likelihood for each row
    lik_binomial_i = lik_binomial, # data likelihood for each row
    sd_i           = sd_i, # crossalked standard deviation
    stacker_col_id = stacker_col_id, # vector indicating which col of X_xp is a stacker & sum-to-1 (1) or not (0)
    options = list(
      use_priors = 1,      # option1==1 use priors 
      adreport_off = 1,    # option2==1 ADREPORT off
      pixel_random = nugget,     # option3==1 include nugget
      country_random = country_re, # option4==1 country random effects
      NID_random = nid_re, # option5==1 NID random effects
      main_space_effect = as.numeric(main_space_effect),
      main_time_effect  = as.numeric(main_time_effect),
      main_age_effect   = as.numeric(main_age_effect),
      full_interacting_effect = as.numeric(full_interacting_effect)),
    prior_log_pixelre_sigma = read_inla_prior_sigma(nugget_prior),
    prior_log_cre_sigma = read_inla_prior_sigma(ctry_re_prior),
    prior_log_nidre_sigma = read_inla_prior_sigma(nid_re_prior),
    prior_matern = spde_list$prior,
    fconstraints = tmb_cov_constraint(colnames(X_xp), cov_constraints),
    aggweight_k = d$agg_weight
  )
  
  # Set staring values for parameters
  Parameters <- list(alpha_j          = fe_start,  # FE parameters alphas
                     logtau           = spde$param.inla$theta.mu[1], # Matern/AR tau
                     logkappa         = spde$param.inla$theta.mu[2], # Matern Range
                     trho             = 0.95,                          # temporal rho
                     zrho             = 0.95,                          # 3rd dimension of GP rho (TODO)
                     log_pixelre_sigma = -1,                            # log(SD) of the normal nugget term
                     log_cre_sigma    = -1,                            # log(SD) of the normal country intercept term (later add slopes as vector)
                     log_nidre_sigma  = -1,                            # log(SD) of the normal NID intercept term 
                     log_gauss_sigma  = -1,                            # log(SD) of normal model
                     Epsilon_stz      = Epsilon_stz,                   # Random Effects: GP locations
                     Epsilon_s        = Epsilon_s,                     # Random Effects: GP locations
                     Epsilon_t        = Epsilon_t,                     # Random Effects: GP locations
                     Epsilon_z        = Epsilon_z,                     # Random Effects: GP locations
                     pixel_re         = rep(0,max(pixel_re_vec)+1),                # Random Effects: Nugget Values
                     cntry_re         = rep(0,nrow(cntry_re_map)),     # Random Effects Values of country random effects (later add in slope stuff)
                     nid_re           = rep(0,max(nid_re_vec)+1))      # Random Effects Values of nid random effects (later add in slope stuff)
  
  
  # put bounds on parameters (Note, this is especially important for rhos)
  L  <- c(rep(-10,ncol(X_xp)),-10,-10,-99,-99,-10,-10,-10) ## updated the rho limits from .99999 since I transformed them in the cpp 
  U  <- c(rep( 10,ncol(X_xp)), 10, 10, 99, 99, 10, 10, 10)
  L[which(stacker_col_id==1)] <- -Inf
  U[which(stacker_col_id==1)] <- Inf
  pn <- c(rep('alpha_j',ncol(X_xp)),'logtau','logkappa','trho','zrho','log_pixelre_sigma','log_cre_sigma','log_nidre_sigma')
  names(L) <- names(U) <- pn
  
  # return the list
  return(list(Data         = Data,
              Parameters   = Parameters,
              cs_df        = cs_df,
              clamper      = clamper,
              coords       = coords,
              mesh         = mesh_s,
              cntry_re_map = cntry_re_map,
              L            = L,
              U            = U))

}




#' @title Fit a TMB MBG model
#' @description Fit a TMB MBG model and pull jointPrecision report
#' @author Roy Burstein
#'
#' @param lbdcorerepo core repo location
#' @param cpp_template name of cpp template file within ./<lbdcorerepo>/mbg_central/
#' @param tmb_input_stack object that results from build_mbg_data_stack_tmb() or build_mbg_data_stack(...,tmb=TRUE)
#' @param ADmap_list map parameter for ignoring parameters
#' @param control_list pass control list to nlminb()
#' @param optimizer which software to use for optimization (optim, or nlminb)
#' @param sparse_ordering boolean: should the ADfun be adjusted before fitting
#'   so that the output SDreport object has a sparse ordering? This option
#'   requires the metis install for TMB.
#'
#' @return list of tmb model objects: ADfun, opt, sdrep
#'
#' @useDynLib mbg_tmb_model
#'
#' @export
#'
#' @note TODO support for other data models, sum to one constraint, country random effects
fit_mbg_tmb <- function(lbdcorerepo     = core_repo,
                        cpp_template    = 'mbg_tmb_model',
                        tmb_input_stack = input_data,
                        ADmap_list      = NULL,
                        control_list    = NULL,
                        optimizer       = 'nlminb',
                        sparse_ordering  = TRUE,
                        seed            = NULL
                        ){
  
  set.seed(seed)
  increment_seed(seed)

  # compile the cpp file and dynload it
  message('compiling template')
  TMB::compile(sprintf('%s/mbg_central/%s.cpp', lbdcorerepo, cpp_template))
  dyn.load( TMB::dynlib(sprintf('%s/mbg_central/%s', lbdcorerepo, cpp_template)) )
  
  # deal with parallelization
  threads <- system('echo $OMP_NUM_THREADS', intern = TRUE)
  if(threads != '') {
    message(sprintf('Detected %s threads in OMP environmental variable.',threads))
    TMB::openmp(as.numeric(threads))
  } else {
    message('Did not detect environmental OMP variable, defaulting to 4 cores. \n
             You can set this using OMP_NUM_THREADS or when launching singularity image.')
    TMB::openmp(4)
  }
  
  # set Data flag for Kaspers normalization fix
  tmb_input_stack$Data$flag <- 1 
  # Initialize random effects
  randompars <- c()
  
  
  # Initialize Autodiff function map list (used to fix parameters)
  if(is.null(ADmap_list)) ADmap_list <- list()
  

  if(tmb_input_stack$Data$options$full_interacting_effect==1){
    randompars <- c(randompars,"Epsilon_stz")
  } else{   
    ADmap_list[['Epsilon_stz']]         <- rep(factor(NA),length(tmb_input_stack$Parameters$Epsilon_stz))
  }  
  parlist <- NULL
  if(tmb_input_stack$Data$options$main_space_effect==1) {
    randompars <- c(randompars,"Epsilon_s")
  } else {
    ADmap_list[['Epsilon_s']]           <- rep(factor(NA),length(tmb_input_stack$Parameters$Epsilon_s))
    parlist         <- c(parlist, 'logtau','logkappa')
  }
  if(tmb_input_stack$Data$options$main_time_effect==1) {
    randompars <- c(randompars,"Epsilon_t")
  } else {
    ADmap_list[['Epsilon_t']]           <- rep(factor(NA),length(tmb_input_stack$Parameters$Epsilon_t))
    parlist         <- c(parlist, 'trho')
  }
  if(tmb_input_stack$Data$options$main_age_effect==1) {
    randompars <- c(randompars,"Epsilon_z")
  } else {
    ADmap_list[['Epsilon_z']]           <- rep(factor(NA),length(tmb_input_stack$Parameters$Epsilon_z))
    parlist         <- c(parlist, 'zrho')
  }

  #If no z-col, set zrho to NA (If there is no z-col, Epsilon_stz is 2 dimensions)
  if(length(dim(tmb_input_stack$Parameters$Epsilon_stz))==2){
    ADmap_list[['zrho']] <- factor(NA)
  }
   #If no t-col, set trho to NA (Epsilon_stz will be 3 dimensions even if length(yl == 1))
  if(dim(tmb_input_stack$Parameters$Epsilon_stz)[2]==1){
    ADmap_list[['trho']] <- factor(NA)
  }
  
  if(tmb_input_stack$Data$options$full_interacting_effect==0){
    for(par in parlist)
      ADmap_list[[par]] <- factor(NA)
  }

  if(tmb_input_stack$Data$options$pixel_random==1){
    # if pixel option is on add pixel to randompars
    randompars <- c(randompars,'pixel_re')
  } else {
    # if no pixel, add pixel related parameters to ignorelist
    ADmap_list[['log_pixelre_sigma']] <- factor(NA)
    ADmap_list[['pixel_re']] <- rep(factor(NA),length(tmb_input_stack$Parameters$pixel_re))
  }
  
  if(tmb_input_stack$Data$options$country_random==1) {
    # if country RE option is on add cntry_re to randompars
    randompars <- c(randompars,'cntry_re')
  } else {
    # if no country re, add cntry_re related parameters to ignorelist
    ADmap_list[['log_cre_sigma']] <- factor(NA)
    ADmap_list[['cntry_re']] <- rep(factor(NA),length(tmb_input_stack$Parameters$cntry_re))
  }

  if(tmb_input_stack$Data$options$NID_random==1) {
    # if NID RE option is on add nid_re to randompars
    randompars <- c(randompars,'nid_re')
  } else {
    # if no NID RE, add nid_re related parameters to ignorelist
    ADmap_list[['log_nidre_sigma']] <- factor(NA)
    ADmap_list[['nid_re']] <- rep(factor(NA),length(tmb_input_stack$Parameters$nid_re))
  }
  
  if(sum(tmb_input_stack$Data$lik_gaussian_i) == 0){
    # map out model sigma if no gaussian observations
    ADmap_list[['log_gauss_sigma']]    <- factor(NA)
  }
  
  # Print fixed parameters and random parameters to be used in TMB run
  message(paste0('ADMAP_LIST: ',  paste0(names(ADmap_list),collapse=', ')))
  message(paste0('Random Pars: ', paste0(randompars,collapse=', ')))
  
  #If all of the random effects are used, set ADmap_list back to NULL
  if(length(ADmap_list) == 0) ADmap_list <- NULL
  
  # make the AD object
  message('Making AD object')
  obj <- TMB::MakeADFun(
    data       = tmb_input_stack$Data, 
    parameters = tmb_input_stack$Parameters,  
    map        = ADmap_list, 
    random     = randompars, 
    hessian    = TRUE, 
    DLL        = cpp_template
  )
  
  # normalize
  obj <- TMB::normalize(obj, flag = "flag")
  
  # Reduce fill in of sparse Cholesky factorization (requires metis install of TMB)
  if(sparse_ordering){
    TMB::runSymbolicAnalysis(obj)
  }
  message(obj)
  message(obj$par)
  message(control_list)
  # Run optimizer
  message('Running MLE')
  if(optimizer == 'nlminb')
    opt0 <- do.call("nlminb",list(start       =    obj$par,
                                  objective   =    obj$fn,
                                  gradient    =    obj$gr,
                                  lower       =    tmb_input_stack$L, 
                                  upper       =    tmb_input_stack$U, 
                                  control     =    control_list))
  if(optimizer == 'optim')
    opt0 <- do.call("optim",list(par = obj$par, fn = obj$fn, control = control_list, gr = obj$gr, method = 'BFGS'))
  
  # run sdreport to get joint precision of all parameters
  for(i in 1:20) message('Getting Joint Precision')
  SD0 <- TMB::sdreport(obj, getJointPrecision = TRUE, bias.correct = TRUE)

  # return
  return(list(
    ADfun   = obj,
    opt     = opt0,
    sdrep   = SD0,
    fenames = colnames(tmb_input_stack$Data$X_kj)
  ))
  
}





#' @title Multivariate normal draws
#' @description Take multivariate normal draws given a mean vector and precision matrix
#'
#' @param mu vector of parameter means
#' @param prec joint precision matrix
#' @param n.sims number of draws
#'
#' @return length(mu) by n.sims matrix of parameter draws
#' 
#' @export
rmvnorm_prec <- function(mu, prec, n.sims) {
  z <- matrix(rnorm(length(mu) * n.sims), ncol=n.sims)
  L <- Cholesky(prec, super = TRUE)
  L <- as(L, 'Matrix')
  z <- solve(L, z, system = "Lt") ## z = Lt^-1 %*% z
  z <- solve(L, z, system = "Pt") ## z = Pt    %*% z
  z <- as.matrix(z)
  return(mu + z)
}









#' @title Predict MBG from a TMB Model
#'
#' @description Project out to a full sample space defined by the sr argument
#' 
#' @author Roy Burstein
#' 
#' @param samples Number of draws to take
#' @param seed Seed to set for RNG
#' @param model_fit_object object output from function fit_mbg_tmb()
#' @param tmb_input_data input_data output of build_mbg_data_stack_tmb
#' @param fes a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#'  corresponding to column names in d
#' @param sr simple raster object
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of z for analysis (i.e. c(1,2,3,4,5)). Config item z_list. No z-dimension it should just be zero
#' @param covs_list named list of covariate bricks, each should be length(yl) long
#' @param clamp_covs Should covariates be 'clamped' to not predict outside of 
#'   their observed range in the data?
#' @param cov_constraints named int vector. integer vector indexed by covariate 
#'   names, in the format returned by the function 
#'   \code{\link{covariate_constraint_vectorize}}. NOTE: In the current
#'   implementation, priors for fixed effects are set to the same values
#'   regardless of whether the covariate fixed effect is constrained or not. 
#'   Apply constraints with caution.
#' @param use_full_interacting_effect TRUE/FALSE to use an interacting GP - this will be space*time if no z-dimension is present, space*age if only one yar is presented, and space*time*age if all three dimensions exist
#' @param use_space_only_gp TRUE/FALSE to include a space-only GP
#' @param use_time_only_gmrf TRUE/FALSE to include a time-only GMRF
#' @param use_age_only_gmrf TRUE/FALSE to include an age-only GMRF
#' @param coefs.sum1 Logical. Were the coefficients constrained to sum-to-1 in the model fit
#'
#' @return a cell_preds object
#'
#' @export
predict_mbg_tmb <- function(samples,
                            seed             = NULL,
                            tmb_input_stack  = input_data,
                            model_fit_object = model_fit,
                            fes              = all_fixed_effects,
                            sr               = simple_raster,
                            yl               = year_list,
                            zl               = z_list,
                            transform        = 'inverse-logit',
                            covs_list        = cov_list,
                            clamp_covs       = FALSE,
                            cov_constraints = use_global_if_missing("cov_constraints"),
                            use_full_interacting_effect = as.logical(use_gp),
                            use_space_only_gp = as.logical(use_space_only_gp),
                            use_time_only_gmrf = as.logical(use_time_only_gmrf),
                            use_age_only_gmrf = as.logical(use_age_only_gmrf),
                            coefs.sum1 = coefs_sum1) {

  # Pull SD report object
  sdrep     <- model_fit_object$sdrep
  
  # pull a few useful things from the input data stack
  cs_transform         <- tmb_input_stack$cs_df
  mesh                 <- tmb_input_stack$mesh
  cntry_re_map         <- tmb_input_stack$cntry_re_map
  if(clamp_covs == TRUE) {
    clamper            <- tmb_input_stack$clamper
  } else {
    clamper            <- NULL
  }
  stacker_col_id <- tmb_input_stack$Data$stacker_col_id

  # set seed if it is requested
  if(!is.null(seed)) {
    set.seed(seed)
    increment_seed(seed)
  }
  # vector of means
  mu    <- c(sdrep$par.fixed,sdrep$par.random)
  
  # simulate draws
  draws <- rmvnorm_prec(mu = mu , prec = sdrep$jointPrecision, n.sims = samples)
  
  ## separate out the draws
  parnames      <- c(names(sdrep$par.fixed), names(sdrep$par.random))
  
  if (use_full_interacting_effect == T){
    epsilon_stz_draws <- draws[parnames=='Epsilon_stz',]
  }   
  if (use_space_only_gp == T){
    epsilon_s_draws <- draws[parnames=='Epsilon_s',]
  }
  if (use_time_only_gmrf == T){
    epsilon_t_draws <- draws[parnames=='Epsilon_t',]
  }
  if (use_age_only_gmrf == T){
    epsilon_z_draws <- draws[parnames=='Epsilon_z',]
  }
  
  alpha_draws   <- draws[parnames=='alpha_j',]

  # seperate out Z FE draws
  FE_z_draws    <- NULL
  if(length(zl) > 1){
    FE_z_draws    <- alpha_draws[ grepl('FE_z_level__',model_fit_object$fenames),] # separate out z-level fixed effects from other FEs
    alpha_draws   <- alpha_draws[!grepl('FE_z_level__',model_fit_object$fenames),] # remove any z-level fixed effects from other FEs
  }

  
  if(length(zl) > 1)
    if(dim(FE_z_draws)[1] != (length(zl)-1) )
      stop('Incorrect number of fixed effects for levels of z in the z_list')
  
  # names of fes
  tmb_const <- tmb_cov_constraint(model_fit_object$fenames, cov_constraints)
  fes       <- unlist(strsplit(fes, ' \\+ '))
  
  # mask covariates to simple raster
  for(l in 1:length(covs_list)) {
    covs_list[[l]]  <- crop(covs_list[[l]], extent(sr))
    covs_list[[l]]  <- setExtent(covs_list[[l]], sr)
    covs_list[[l]]  <- mask(covs_list[[l]], sr)
  }
  
  # keep only covariates used in model, typically stacking
  covs_list <- covs_list[names(covs_list) %in% fes]
  
  # get coordinates of full projection space
  # Extract admin0 code
  f_orig <- data.table(cbind(xyFromCell(sr, seegSDM:::notMissingIdx(sr)), adm_code=as.vector(sr[seegSDM:::notMissingIdx(sr)])))
  f_orig$t <- f_orig$z <- 1 # set initial time and Z
  f_orig[,tmpord:=1:.N]

  # use the country code dt from input_data to map admin0 code to RE values 
  f_orig <- merge(f_orig,cntry_re_map[,c('adm_code','re_id'),with=FALSE],by='adm_code',all.x=TRUE)
  f_orig <- f_orig[order(tmpord)] # make 100% sure everything is correctly ordered after the merge. 
  f_orig[, re_id := re_id+1 ]  # to deal with indexing which started at 0 in the cpp                        
  f_orig$re_id[is.na(f_orig$re_id)] <- 0 # to deal with countries not in the data
  
  # add time periods and z periods as needed
  grp <- setDT(expand.grid(1:length(yl), 1:length(zl)))
  setnames(grp,c('Var1','Var2'),c('t','z'))
  grp[,group := 1:.N]
  fullsamplespace <- data.table()
  for(g in 1:max(grp$group)){
    tmp <- f_orig
    tmp[,z  := grp$z[grp$group==g]]
    tmp[,t  := grp$t[grp$group==g]]
    tmp[,gp := g]
    fullsamplespace <- rbind(fullsamplespace,tmp)
  }
  fullsamplespace[,idx := 1:.N]

  # pull out covariates in format we expect them
  # a list of length periods with a brick of named covariates inside
  new_cl <- list()
  if(length(covs_list)==0){
    message('No covariates detected, predicting using intercept only.')
  } else {
    message(sprintf('%i covariates detected.',length(covs_list)))
    
    for(p in 1:length(yl)){
      new_cl[[p]] <- list()
      for(n in names(covs_list)){
        if(dim(covs_list[[n]])[3]==1) { # synoptic mean covariates
          new_cl[[p]][[n]] <- covs_list[[n]]
        } else if (dim(covs_list[[n]])[3]==length(yl)) { # time varying covariates
          new_cl[[p]][[n]] <- covs_list[[n]][[p]]
        } else { # error if there is some other weird non-conforming year thing
          stop(sprintf('Covariate %n is a brick with %i layers, while year_list has %i years',
                       n,dim(covs_list[[n]])[3],length(yl)))
        }
      }
      new_cl[[p]] <- brick(new_cl[[p]])
    }
  }
  
  # get surface locs to project on to
  pcoords        <- cbind(x=fullsamplespace$x, y=fullsamplespace$y) ## used for cov raster extract

  ## setup coords for GP projection. convert coords to spherical if
  ## using spherical modeling mesh. used if you made a mesh on s2
  if(mesh_s$manifold == "S2"){
    gp_coords <- lonlat3D(pcoords[, 1], pcoords[, 2])
  } else {
    gp_coords <- pcoords
  }

  ## define grouping across periods
  groups_periods <- fullsamplespace$gp
                  
  # extract cell values  from covariates, deal with timevarying covariates here
  cov_vals <- list()
  for(z in 1:length(zl)){
    cov_vals[[z]] <- list()
    for(p in 1:length(yl)){
      if(length(fes)>0) {
        
        # raster extract and keep only fes
        cov_vals[[z]][[p]] <- raster::extract(new_cl[[p]], pcoords[1:nrow(f_orig),])
        cov_vals[[z]][[p]] <- cov_vals[[z]][[p]][,colnames(cov_vals[[z]][[p]]) %in% c(fes)]
        
        # If there is only a single covariate, convert from vector to matrix
        if( (length(covs_list)==1) & !('matrix' %in% class(cov_vals[[z]][[p]]))){
          cov_vals[[z]][[p]] <- matrix(cov_vals[[z]][[p]], ncol=1)
        }

        # transform raw covariate values (center scaled) if needed (i.e. if cs_tranform is not 1 0 for that variable)
        cov_vals[[z]][[p]] <- centreScale(cov_vals[[z]][[p]],cs_transform) 
        
        # clamp covariates if clamper is not null
        if(!is.null(clamper)){
         # message('Clamping')
          for(fe in fes){
            tmpvec <- cov_vals[[z]][[p]][,colnames(cov_vals[[z]][[p]])==fe]
            mn <- as.numeric(clamper[,fe,with=FALSE][1])
            mx <- as.numeric(clamper[,fe,with=FALSE][2])
            tmpvec[tmpvec<mn] <- mn
            tmpvec[tmpvec>mx] <- mx
            cov_vals[[z]][[p]][,colnames(cov_vals[[z]][[p]])==fe] <- tmpvec
          }
        }
        # add an intercept
        cov_vals[[z]][[p]] <- cbind(int = 1, cov_vals[[z]][[p]])
      } else {
        # if no covariates just do intercept only
        cov_vals[[z]][[p]] <- cbind(int = rep(1,nrow(f_orig)))
      }
      # if there is a z column, add on those fixed effects indicators
      if(length(zl) > 1){
        tmpzmat <- matrix(0,ncol = (length(zl)-1), nrow = nrow(f_orig))
        colnames(tmpzmat) <- paste0('FE_z_level__',2:length(zl))
        for(zz in 2:length(zl))
          if(z == zz) 
            tmpzmat[,paste0('FE_z_level__',zz)] <- 1
        cov_vals[[z]][[p]] <- cbind(cov_vals[[z]][[p]], tmpzmat)
      }
    }
  }
  
  # covariate values by alpha draws
  l_vals <- list()
  for(z in 1:length(zl)){
    l_vals[[z]] <- list()
    for(p in 1:length(yl)) {
      # First apply constraints and then sum-to-one (note: if using sum-to-one do not also apply other constraints to those parameters)
      l_vals[[z]][[p]] <- cov_vals[[z]][[p]] %*% apply_sum_to_one(stacker_col_id,apply_constraints(tmb_const, rbind(alpha_draws,FE_z_draws)))
    }  
      
  }
  cell_l <- do.call("rbind",unlist(l_vals, recursive = FALSE))
  
  ## use inla helper functions to project the spatial effect.
  A.pred_stz <- inla.spde.make.A(
    mesh  = mesh_s,
    loc   = gp_coords,
    group = groups_periods)
  
  # make a projection matrix from data to s mesh
  A.pred_s <- inla.spde.make.A(
    mesh  = mesh_s,
    loc   = gp_coords)
  
  ### values of GP ST surface at each cell (long by nperiods)
  # if we have multiple zs then do this by z since its possible to throw a SuiteSparse 'Problem too large' error here. 
  if(use_full_interacting_effect){
    if(length(zl) > 1){
      cell_stz <- list()
      for(zz in 1:length(zl)){
        cell_stz[[zz]] <- as.matrix(A.pred_stz[(which(fullsamplespace$z==zz)),] %*% epsilon_stz_draws)
      }
      cell_stz <- do.call('rbind',cell_stz)
    } else{
      cell_stz <- as.matrix(A.pred_stz %*% epsilon_stz_draws)
    }
  }
  
  ### values of GP S surface at each cell (long by nperiods)
  if(use_space_only_gp)  {
    if(length(zl) > 1){
      cell_s <- list()
      for(zz in 1:length(zl)){
        cell_s[[zz]] <- as.matrix(A.pred_s[(which(fullsamplespace$z==zz)),] %*% epsilon_s_draws)
      }
      cell_s <- do.call('rbind',cell_s)
    } else{
      cell_s <- as.matrix(A.pred_s %*% epsilon_s_draws)
    }
  }
  
  #Get raster & raster*year vectors for use in GPT & GPZ surface creation 
  cell_idx <- seegSDM:::notMissingIdx(sr)
  s.vec <- 1:length(sr[cell_idx])  ## vector of spatial cell count
  st.vec <- rep(s.vec, length(yl)) ## expand s vec in time
  
  ### values of GP T surface at each cell, across agebins
   if(use_time_only_gmrf) {
   cell_t<-list()
     for(z in 1:length(zl)) {
       cell_t[[z]]<-list()
       for(t in 1:length(yl)){
         cell_t[[z]][[t]] <- sapply(
           epsilon_t_draws[t, ],
           function(it) rep(it, length(s.vec))
         )
       }
       cell_t[[z]] <- do.call('rbind',cell_t[[z]])
     }
   cell_t <- do.call('rbind',cell_t)
 }
  
  ### values of GP Z surface at each cell  
   if(use_age_only_gmrf) {
   cell_z<-list()
     for(z in 1:length(zl)) {
       cell_z[[z]]<-list()
       for(t in 1:length(yl)){
         cell_z[[z]][[t]] <- sapply(
           epsilon_z_draws[z, ],
           function(it) rep(it, length(s.vec))
         )
       }
       cell_z[[z]] <- do.call('rbind',cell_z[[z]])
     }
   cell_z <- do.call('rbind',cell_z)
 }
   # add the pixel-level re if needed (i.e. pixel-level re parameter was not mapped out)

  cell_nug <- matrix(0L, nrow = dim(cell_l)[1], ncol = dim(cell_l)[2])
  if('log_pixelre_sigma' %in% parnames){
    if(exists('no_nugget_predict') & no_nugget_predict==TRUE){
      message('Pixel-level random effect not included in predict')
    } else {
      message('Adding pixel-level random effect')
      for(s in 1:samples)
        cell_nug[,s] <- rnorm(dim(cell_nug)[1],0,exp(draws[parnames=='log_pixelre_sigma',])[s])
    }
  }
  
  # add the country random intercept if it was estimated in the model
  cell_cre_int <- matrix(0L, nrow = dim(cell_l)[1], ncol = dim(cell_l)[2])
  if('log_cre_sigma' %in% parnames){
    message('adding country random intercept')
    cre <- data.table(draws[parnames=='cntry_re',])
    cre[, re_id := 1:.N]
    cre <- merge(fullsamplespace,cre,by='re_id',all.x=TRUE)
    cre <- cre[order(idx)]
    cre <- cre[,grep('V',colnames(cre)),with=FALSE]
    if(all(dim(cell_cre_int) == dim(cre))){
      cell_cre_int <- as.matrix(cre)
      rm(cre)
    } else {
      stop('CHECK COUNTRY RE DIMENSIONS')
    }
    
  }
  
  # add together linear and st components
  pred_tmb <- cell_l +cell_nug + cell_cre_int
  
  if(use_full_interacting_effect)             pred_tmb <- pred_tmb + cell_stz
  if(use_space_only_gp)  pred_tmb <- pred_tmb + cell_s
  if(use_time_only_gmrf) pred_tmb <- pred_tmb + cell_t
  if(use_age_only_gmrf)  pred_tmb <- pred_tmb + cell_z
  
  # transform
  if(transform=='inverse-logit') { 
    pred_tmb <- plogis(as.matrix(pred_tmb))
  } else {
    pred_tmb <- eval(parse(text=sprintf('%s(as.matrix(pred_tmb))',transform)))
  }
  
  # if there is more than one z, then return a list of length zl cell_preds
  if(length(zl) > 1){
    pred_tmb_list <- list()
    chunklength <- dim(pred_tmb)[1]/length(zl) 
    for(z in 1:length(zl))
      pred_tmb_list[[z]] <- pred_tmb[((z-1)*chunklength+1):(chunklength*z),1:samples]
    pred_tmb <- pred_tmb_list
  }
  
  # return the predicted cell_pred object
  return(pred_tmb)

}



#' @title Get fitted model parameters from TMB object
#' @description Make a nice table of fitted model parameters from a geostat TMB object
#' @author Roy Burstein
#'
#' @param model_fit fitted TMB object that comes out of fit_mbg_tmb()
#' @param exp_fixed_effects Boolean, should the fixed effects be exponentiated (if model was fit with logit link). Defaults to TRUE
#' @param transform_hyperparams Boolean, should hyperparmeters be transformed from fitted to more natural space. Defaults to TRUE
#' @param draws Integer, number of draws to use. Defaults to 1000
#' @param calculate_range Boolean, should we calculate and report range using kappa in draws space. Defaults to TRUE
#' @param calculate_nom_var  Boolean, should we calculate and report nominal variance using kappa and tau in draws space. Defaults to TRUE
#' @param cov_constraints named int vector. integer vector indexed by covariate 
#'   names, in the format returned by the function 
#'   \code{\link{covariate_constraint_vectorize}}. NOTE: In the current
#'   implementation, priors for fixed effects are set to the same values
#'   regardless of whether the covariate fixed effect is constrained or not. 
#'   Apply constraints with caution. 
#'
#' @return formatted data.table object
fitted_param_table_tmb <- function(model_fit,
                                   exp_fixed_effects     = TRUE,
                                   transform_hyperparams = TRUE,
                                   draws                 = 1000,
                                   calculate_range       = TRUE,
                                   calculate_nom_var     = TRUE,
                                   cov_constraints = use_global_if_missing("cov_constraints")) {
  
  # get draws of parameter values
  mu <- model_fit$sdrep$par.fixed
  pm <- model_fit$sdrep$jointPrecision[1:length(mu),1:length(mu)]
  draws <- rmvnorm_prec(mu,pm,draws)

  # deal with given names of parameters
  fn <- names(model_fit$sdrep$par.fixed)
  fn[fn == 'alpha_j'] <- model_fit$fenames

  # Apply constraint transformations
  tmb_const <- tmb_cov_constraint(model_fit$fenames, cov_constraints)
  draws[names(model_fit$sdrep$par.fixed) == "alpha_j",] <-
    apply_constraints(tmb_const, draws[names(model_fit$sdrep$par.fixed) == "alpha_j",])

  # Transform fixed effects
  if(exp_fixed_effects == TRUE){
    draws[which(fn %in% model_fit$fenames),] <- exp(draws[which(fn %in% model_fit$fenames),])
  }

  # Get the range parameter
  if(calculate_range == TRUE){
    # see equation 6.16 (pg 196) of Blangiardo and Cameletti Book 
    ranger <- sqrt(8) / exp(draws[which(fn == 'logkappa'),])
    draws  <- rbind(draws, ranger)
    fn     <- c(fn, 'range')
  }
  
  # Get the nominal variance parameter
  if(calculate_nom_var == TRUE){
    # see equation 6.17 (pg 196) of Blangiardo and Cameletti Book 
    nominal_variance <- 1 / (4 * pi * (exp(draws[which(fn == 'logkappa'),]))^2 * (exp(draws[which(fn == 'logtau'),]))^2)
    draws <- rbind(draws, nominal_variance)
    fn    <- c(fn, 'nominal_variance')
  }
  
  # transform hyperparmeters
  if(transform_hyperparams == TRUE){
    draws[which(fn == 'logtau'),]             <- exp(draws[which(fn == 'logtau'),])
    draws[which(fn == 'logkappa'),]           <- exp(draws[which(fn == 'logkappa'),])
    draws[which(fn == 'log_pixelre_sigma'),]   <- exp(draws[which(fn == 'log_pixelre_sigma'),])
    draws[which(fn == 'log_cre_sigma'),]      <- exp(draws[which(fn == 'log_cre_sigma'),])
    draws[which(fn == 'log_nidre_sigma'),]    <- exp(draws[which(fn == 'log_nidre_sigma'),])
    
    fn[fn == 'logtau']           <- 'tau'
    fn[fn == 'logkappa']         <- 'kappa'
    fn[fn == 'log_pixelre_sigma'] <- 'nugget_SD'
    fn[fn == 'log_cre_sigma']    <- 'country_RE_SD'
    fn[fn == 'log_nidre_sigma']  <- 'NID_RE_SD'
    
    draws[which(fn == 'zrho'),] <- (exp( draws[which(fn == 'zrho'),] ) - 1) / (exp( draws[which(fn == 'zrho'),] ) + 1)
    draws[which(fn == 'trho'),] <- (exp( draws[which(fn == 'trho'),] ) - 1) / (exp( draws[which(fn == 'trho'),] ) + 1)
    fn[fn == 'zrho'] <- 'age_rho'
    fn[fn == 'trho'] <- 'year_rho'
    
  }
  
  # summarize draws and clean up data table
  su <- data.table(t(apply(draws,1,quantile,c(0.025,0.500,0.975))))
  su[, fn := fn]
  colnames(su) <- c('lower','median','upper','param_name')
  su <- su[,c('param_name','median','lower','upper'),with=FALSE]
  
  # return the final table
  return(su)
}

#' @title Read INLA SD prior for TMB
#'
#' @description Read in a prior specification that is suited for INLA and make 
#'   it TMB readable.
#'
#' @param prior_string character, character vec of length 1 specifying priors
#'
#' @return List specifying a TMB prior, containing three elements:
#'   - type: Is the prior normal, loggamma, or pc.prec
#'   - par1: The first shape parameter. In the lognormal case, the mean
#'   - par2: The second shape parameter. In the lognormal case, the variance
#'
read_inla_prior_sigma <- function(prior_string){
  prior_list <- eval(parse(text=prior_string[1]))
  if(!(prior_list$prior %in% c("normal", "loggamma", "pc.prec"))){
    stop("TMB implementation only supports normal, loggamma, or PC priors for 
         SD parameters.")
  }
  return(list(
    type = prior_list$prior,
    par1 = prior_list$param[1],
    par2 = prior_list$param[2]
  ))
}

#' @title Read INLA Matern GP priors for TMB
#'
#' @description Read in a prior specification from config and make 
#'   it TMB readable.
#'
#' @param prior_string character, character vec of length 1 specifying priors
#'
#' @return List containing (1) spde object and (2) list specifying a TMB prior, 
#'   containing three elements:
#'   - type: Is the prior pc or nonpc (i.e. normal)
#'   - par1: Vector of length 2 for the first parameter. In the nonpc case, 
#'     corresponds to mean and precision for logtau. In the pc case, 
#'     corresponds to range0 and prange.
#'   - par2: Vector of length 2 for the first parameter. In the nonpc case, 
#'     corresponds to mean and precision for logkappa. In the pc case, 
#'     corresponds to sigma0 and psigma.
read_inla_prior_matern <- function(prior_string, mesh_s){
  prior_list <- eval(parse(text=prior_string[1]))
  
  spde_list <- build_spde_prior(prior_list, mesh_s, st_gp_int_zero=FALSE)
  spde_prior <- spde_list$spde_prior
  spde <- spde_list$spde
  
  if(spde_prior$type == "nonpc") {
    par1 <- c(spde$param.inla$theta.mu[1], spde$param.inla$theta.Q[1,1])
    par2 <- c(spde$param.inla$theta.mu[2], spde$param.inla$theta.Q[2,2])
  } else {
    par1 <- spde_prior$prior$range
    par2 <- spde_prior$prior$sigma
  }
  
  
  return(list(spde=spde,
              prior=list(
                type = prior_list$type,
                par1 = par1,
                par2 = par2)
  ))
}

#' @title Modify constraints to fit TMB formatting
#' 
#' @author Neal Marquez
#' 
#' @description Optionally add constraints to fixed effects in the TMB 
#'   optimization model. In the TMB model, a constraint label of '0' indicates
#'   that the fixed effect is unconstrained, a label of '1' constrains the 
#'   above zero, and and a label of '-1' constrains the variable below zero.
#'
#' @param fes character, fixed effects in model
#' @param cov_constraints named int vector output from the 
#'   `covariate_constraint_vectorize()` function. Specifies how to constrain 
#'   each fixed effect.
#' @param zl int, additional constraints to pad on which will be 0
#' 
tmb_cov_constraint <- function(
  model_fes,
  cov_constraints = use_global_if_missing("cov_constraints")
  ){
  tmb_const <- sapply(unlist(strsplit(model_fes, " \\+ ")), function(m){
    if(m %in% names(cov_constraints)){
      x <- unname(cov_constraints[m])
    }
    else{
      x <- 0
    }
    x
  })

  return(tmb_const)
}

#' @title Apply constraints to fitted value draws
#' 
#' @author Neal Marquez
#' 
#' @description Given draws of fixed effect values generated from a fitted TMB
#'   model and a list of constraints applied to fixed effects in that model, 
#'   apply transformations on constrained fixed effects to reproduce how they
#'   were incorporated in the model (constraining them above or below zero). If
#'   a fixed effect had a constraint label of 0 (unconstrained, the default),
#'   the untransformed draws will be returned.
#' 
#' @param tmb_const int vector, tmb prepped constraints
#' @param alpha_draws matrix, fitted draws of coefficients
#' 
#' @return matrix of transformed beta coefficients for fixed effect draws
#' 
apply_constraints <- function(tmb_const, FE_draws){
  Nfe <- nrow(FE_draws)
  
  FEdraws_const <- sapply(1:Nfe, function(j){
    X <- FE_draws[j,]
    if(tmb_const[j] == 1){
      X <- exp(X)
    }
    else if(tmb_const[j] == -1){
      X <- -exp(X)
    }
    
    return(X)
  })
  
  return(t(FEdraws_const))
}

#' @title Apply sum-to-one to fitted value draws
#' 
#' @description Given draws of fixed effect values generated from a fitted TMB
#'   model and a vector of which columns are stackers with a sum-to-one in that 
#'   model, apply transformations on constrained fixed effects to reproduce how 
#'   they were incorporated in the model (constraining them sum-to-one). See
#'   https://en.wikipedia.org/wiki/Dirichlet_distribution#Gamma_distribution for
#'   details on how a transformation of (log) gamma distributed RVs can be 
#'   transformed into RVs with a dirichlet distribution. If a stacker column has 
#'   a label of 0 (unconstrained, the default) or for any non-stacker columns, 
#'   the untransformed draws will be returned.
#' 
#' @param stacker_col_id int vector, which columns have sum-to-one (1) or not (0)
#' @param FE_draws matrix, fitted draws of coefficients
#' 
#' @return matrix of transformed beta coefficients for fixed effect draws
#' 
#' @export
apply_sum_to_one <- function(stacker_col_id, FE_draws){
  unnormalized_draws <- t(exp(FE_draws[stacker_col_id==1,]))
  normalized_draws <- unnormalized_draws/rowSums(unnormalized_draws)
  FE_draws[stacker_col_id==1,] <- t(normalized_draws)
  
  return(FE_draws)
}
