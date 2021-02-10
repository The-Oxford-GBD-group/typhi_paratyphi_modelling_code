#' @title Build Stack for TMB MBG model
#' @description Organize Data and Parameter Stacks to fit a TMB MBG model
#' @author Roy Burstein
#'
#' @param d prepped model frame with no NAs
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of zcol for analysis (i.e. ages c(1,2,3,4,5). Must be integers starting with 1)
#' @param fes a string of model fixed effects of form 'var1 + var2 + var3' or as string vector
#' corresponding to column names in d
#' @param indic indicator name, corresponding to the response variable in d
#' @param country_re TRUE/FALSE include country re. If true and there is a zcol then there will be random slope as well (todo)
#' @param nid_re TRUE/FALSE include re on survey. Need nid column in the data frame. defaults to use_nid_res from config.
#' @param exclude_cs character vector of covariates to exclude from centrescaling
#' @param mesh_s an inla mesh object
#' @param spde_prior String containing a list. Specifies the type of prior used
#' for the Matern model parameters. If type = "nonpc" then use non pc
#' prior. User can specify nominal prior means: spde_prior$prior$range.nominal
#' and spde_prior$prior$variance.nominal. Defaults are INLA defaults, that is
#' 20\% of the range of the mesh and 1, respectively. If type = "pc" then use
#' pc prior.  User can specify spde_prior$prior$range = (range0, Prange) i.e.
#' P(range < range0) = Prange and spde_prior$prior$sigma = (sigma0, Psigma)
#' i.e. P(sigma > sigma0) = Psigma. Defaults for Prange and Psigma are 0.05.
#' Default for range0 is 5\% of the range of the mesh and sigma0=3.
#' @param nugget Logical. If TRUE, include a nugget effect
#' @param ycol column name of y values, defaults to year
#' @param zcol column name of z values associated with zl.
#' @param shapefile_version character. Version of shape file to use.
#' @param scale_gaussian_variance_N Logical. Do you want to scale gaussian variance by sample size?
#' @param cov_constraints named int vector. integer vector indexed by covariate
#' names, in the format returned by the function
#' \code{\link{covariate_constraint_vectorize}}. NOTE: In the current
#' implementation, priors for fixed effects are set to the same values
#' regardless of whether the covariate fixed effect is constrained or not.
#' Apply constraints with caution.
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
#' @export
#' TODO: Allow users to set priors, allow different data model (gaussian to start), country random effects
#'
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
  if (length(list(...)) > 0) {
    list2env(list(...), environment())
  }

  # fix row_id to increment by 1
  d <- unique(select(d, row_id)) %>%
    mutate(row_id_transf = 1:n()) %>%
    right_join(d, by = "row_id")
  d <- setDT(d)

  # zcol
  if (!zcol %in% colnames(d)) {
    message("No Z column detected")
    d[[zcol]] <- 0
  }
  if (!all(unique(d[[zcol]]) %in% zl)) {
    message("WARNING: zl and d[[zcol]] do not completely match up.. ")
    # check that we there arent values in zcol not matching z_list
    d[, dropz := !get(zcol) %in% zl]
    if (any(d$dropz != FALSE)) {
      message(sprintf("WARNING: Detected some z values in zcol (%s) which were not in the z_list", zcol))
      message(sprintf("WARNING: Due to this, dropping %i rows from the input data", sum(d$dropz == TRUE)))
      print(table(d$dropz, d$age))
      d <- subset(d, dropz == FALSE)
      d[, dropz := NULL]
    }
  }

  # make a fake data point with no weight for the max period and Z to fill out the GP
  d <- rbind(d, d[1, ])
  d[[zcol]][nrow(d)] <- max(zl)
  d[["period"]][nrow(d)] <- length(yl)
  d[["weight"]][nrow(d)] <- 0
  d[["row_id_transf"]][nrow(d)] <- max(d$row_id_transf) + 1


  # look for z dimension
  num_z <- 1 # TODO get this from the z-list
  if (length(zl) > 1) {
    message(sprintf("More than one unique %s found, initiating Z in the GP", zcol))
    num_z <- length(zl)

    # set A proj grouping. The ordering here must match the ordering of epsilon_stz in the template
    grp <- setDT(expand.grid(1:length(yl), 1:max(zl)))
    setnames(grp, c("Var1", "Var2"), c("period", zcol))
    grp[, group := 1:.N]
    d <- merge(d, grp, by = c("period", zcol), all.x = TRUE) # warning this may re-order things, so do not use d stuff from above

    # reorder d by row_id
    d <- d[order(row_id_transf), ]
  } else {
    # set Aproj grouping to period if there is but one z value
    d$group <- d$period
  }

  # coordinates at data points. these are passed to TMB in long,lat so
  # we keep them and create another set for 3d coords
  coords <- cbind(d$longitude, d$latitude)

  # if we have  mesh on s2, first convert coords to spherical to project to mesh
  data.locs <- coords ## long, lat
  if (mesh_s$manifold == "S2") {
    ## then the mesh is on the sphere and we need to use 3d coords
    data.locs <- lonlat3D(data.locs[, 1], data.locs[, 2])
  }


  # make a projection matrix from data to st mesh
  A.proj_stz <- inla.spde.make.A(
    mesh = mesh_s,
    loc = data.locs,
    group = d$group
  )

  # make a projection matrix from data to s mesh
  A.proj_s <- inla.spde.make.A(
    mesh = mesh_s,
    loc = data.locs
  )

  A.proj_t <- d[[ycol]] - min(yl)
  A.proj_z <- d[[zcol]] - min(zl)

  # make a clean design matrix. make sure all fes appear in d
  fes <- unlist(strsplit(fes, " \\+ "))
  if (!all(fes %in% names(d))) {
    stop("Check your fes argument, not all covariate names appear in d.")
  }
  if (length(fes) != 0) {
    X_xp <- as.matrix(cbind(int = 1, d[, c(fes), with = FALSE]))
  } else {
    X_xp <- as.matrix(cbind(int = rep(1, nrow(d))))
  }

  # add in age fixed effects
  # TODO eventually do country random slopes for each FE level of Z
  if (num_z > 1) {
    message(sprintf("Adding fixed effects for levels of zcol (%s)", zcol))
    for (z in 2:num_z) {
      X_xp <- cbind(X_xp, d[[zcol]] == z)
    }
    colnames(X_xp) <- c(colnames(X_xp)[colnames(X_xp) != ""], paste0("FE_z_level__", 2:num_z))
    exclude_cs <- c(exclude_cs, paste0("FE_z_level__", 2:num_z))
  }

  # cs_df. imports seegMBG
  cs_df <- getCentreScale(X_xp, exclude = c("int", exclude_cs))
  X_xp <- centreScale(X_xp, df = cs_df)

  # get data range in case we want to clamp for prediction later
  clamper <- data.table(apply(X_xp, 2, range))

  # create stacker_col_id to identify which columns in stacker have sum-to-1
  if (coefs.sum1 == TRUE) {
    stacker_col_id <- ifelse(colnames(X_xp) %in% stacker_names, 1, 0)
  } else {
    stacker_col_id <- rep(0, ncol(X_xp))
  }

  # sort nugget and RE indicators
  nugget <- as.numeric(as.logical(nugget))
  country_re <- as.numeric(as.logical(country_re))
  nid_re <- as.numeric(as.logical(nid_re)) # these do not get used in prediction

  # obtain d_i (unique observations)
  d_i <- d[first_entry == 1, ]

  # check there is more than one observed country or nid if REs for those are set
  if (length(unique(d_i$country)) == 1 & country_re == TRUE) {
    message("WARNING: Only found one unique country in this data frame, so turning off country random effects.")
    country_re <- FALSE
  }
  if (length(unique(d_i$nid)) == 1 & nid_re == TRUE) {
    message("WARNING: Only found one unique NID in this data frame, so turning off NID random effects.")
    nid_re <- FALSE
  }

  # make a table mappind the Admin0 code to a unique country random effect
  md <- get_location_code_mapping(shapefile_version = shapefile_version)
  mdsub <- md[ihme_lc_id %in% unique(as.character(d_i$country)), ]
  if (nrow(mdsub) != length(unique(as.character(d$country)))) {
    message(sprintf("get_location_code_mapping() COUNTRY NAMES: %s", paste(sort(mdsub$ihme_lc_id), collapse = ", ")))
    message(sprintf("IN DATA COUNTRY NAMES: %s", paste(sort(unique(as.character(d$country))), collapse = ", ")))
    stop("get_location_code_mapping() and countries in data not of matching lengths")
  }
  cntry_re_map <- data.table(
    country = mdsub$ihme_lc_id,
    adm_code = mdsub$ADM_CODE,
    re_id = 0:(nrow(mdsub) - 1)
  )
  cntry_re_vec <- cntry_re_map$re_id[match(as.character(d_i$country), cntry_re_map$country)]

  # make an nid_re mapping table
  nid_re_map <- unique(select(d_i, nid)) %>%
    mutate(re_id = 0:(n() - 1))
  nidEFF <- select(d_i, nid) %>% left_join(nid_re_map, by = "nid")
  nid_re_vec <- nidEFF$re_id
  countries_with1nid <- select(d_i, country, nid) %>%
    group_by(country) %>% # group by country to see...
    mutate(nidCount = n()) %>% # the number of nid units per country
    filter(nidCount == 1) %>%
    ungroup() %>%
    select(country) %>%
    unique()
  if (country_re == TRUE & nid_re == TRUE & nrow(countries_with1nid) > 0) {
    message(paste(
      "WARNING! The following countries", countries_with1nid$country,
      "have only 1 NID. If you encounter convergence problems, you 
                  may want to remove NID random effects."
    ))
  }

  # Pixel re
  pixelEFF <- unique(select(d, pixel_id)) %>%
    mutate(pixel_id_transf = 1:n()) %>%
    right_join(d, by = "pixel_id")

  pixel_re_vec <- pixelEFF$pixel_id_transf - 1

  # set GP RE array, or matrix depending on if we have a z dimension
  if (num_z > 1) {
    Epsilon_stz <- array(0, dim = c(mesh_s$n, length(yl), num_z))
  } else {
    Epsilon_stz <- array(0, dim = c(mesh_s$n, length(yl)))
  }

  Epsilon_s <- rep(0, mesh_s$n)
  Epsilon_t <- rep(0, length(yl))
  Epsilon_z <- rep(0, num_z)


  # set up vectors of model family
  # look for convention in the data of lik_fam_<<binom,gauss>>, if not there, default to config family
  lik_gaussian <- lik_binomial <- rep(0, nrow(d_i))
  if (("lik_fam_binom" %in% names(d_i)) & ("lik_fam_gauss" %in% names(d_i))) {
    lik_gaussian <- d_i$lik_fam_gauss
    lik_binomial <- d_i$lik_fam_binom
    message(sprintf(
      "Found row specific data likelihood indicators, will use those. %i rows binom, %i rows gauss",
      sum(lik_binomial), sum(lik_gaussian)
    ))
  } else {
    if (indicator_family == "binomial") {
      lik_binomial <- rep(1, nrow(d_i))
      message("Using indicator family binomial for all rows")
    } else if (indicator_family == "gaussian") {
      lik_gaussian <- rep(1, nrow(d_i))
      message("Using indicator family gaussian for all rows")
    }
  }

  if (any(lik_gaussian + lik_binomial != 1)) {
    stop("Not all rows in your data have been assigned a model (binom or gauss), or some have been assigned multiple!")
  }

  # also look for sd if already exists in the data for the gauss rows to use
  # This is useful for crosswalked values with some data uncertainty, convention is variable named sd_<<INDICATOR>>
  sd_i <- rep(0, nrow(d_i))
  if (paste0("sd_", indicator) %in% names(d)) {
    message("Found SD estimates to use for crosswalked values.")
    sd_i <- d_i[[paste0("sd_", indicator)]]
    sd_i[is.na(sd_i)] <- 0
  }


  # run a quick regression to get starting values for the fixed effects
  # This can speed up model fitting if iterations are slow.
  # Note this fit is currently assuming binomial
  # Note sometimes the starting values can strongly impact results
  if (all(lik_binomial == 1) & coefs.sum1 != TRUE) {
    message("LM for starting fe vals")
    y <- (d_i[[indic]][lik_binomial == 1] + .0001) / d_i$N[lik_binomial == 1]
    y[y <= 0] <- 0.001
    y[y >= 1] <- 0.999
    fe_start <- round(unname(lm(qlogis(y) ~ -1 + X_xp[d$first_entry == 1 & lik_binomial == 1, ])$coefficients), 4)
  } else {
    message("Default starting fe vals")
    fe_start <- rep(0, ncol(X_xp))
  }
  message(sprintf("starting values for fixed effects: %s", paste0(fe_start, collapse = ", ")))


  # cannot allow a gaussian likelihood to also have a nugget in the linear term, it leads to issues
  if (nugget == 1 & all(lik_gaussian == 1)) {
    message("WARNING:: Nugget in all gaussian model leads to identifiability issues. Removing nugget for you.")
    nugget <- 0
  }


  # check if user wants to scale gaussian variance by N, if not set them all to one in the gaussian rows
  # TODO is this still true when it is a pixel re?
  n_i <- d_i$N
  if (scale_gaussian_variance_N == FALSE & sum(lik_gaussian) > 1) {
    message("Not scaling gaussian error by N since scale_gaussian_variance_N == FALSE.")
    n_i[lik_gaussian == 1] <- 1
  }

  # if there is only one country in the region, turn off country_re
  if (all(country_re == TRUE & length(get_adm0_codes(reg)) == 1)) {
    message("WARNING: One country in this region, so turning off country random effects")
    country_re <- FALSE
  }

  # print some messages for random effects
  if (nugget == TRUE) message("USING PIXEL RANDOM EFFECTS")
  if (country_re == TRUE) message("USING COUNTRY RANDOM EFFECTS")
  if (nid_re == TRUE) message("USING NID RANDOM EFFECTS")


  # Build SPDE object (using INLA functions) and get prior in TMB readable format
  spde_list <- read_inla_prior_matern(spde_prior, mesh_s)
  spde <- spde_list$spde

  # Construct a list of all data necessary to TMB to fit
  Data <- list(
    num_z = num_z,
    y_i = d_i[[indic]], # Number of observed events in the cluster (N+ in binomial likelihood)
    n_i = d_i$N, # Number of observed exposures in the cluster (N in binomial likelihood)
    c_re_i = cntry_re_vec, # vector of country ids, ( starting at zero because C)
    nid_re_i = nid_re_vec, # vector of survey ids, zero index added in cpp for null effect
    pixel_re_k = pixel_re_vec, # vector of pixel ids
    w_i = d_i$weight, # Data weight for each row
    X_kj = X_xp, # Covariate design matrix
    M0 = spde$param.inla$M0, # SPDE sparse matrix
    M1 = spde$param.inla$M1, # SPDE sparse matrix
    M2 = spde$param.inla$M2, # SPDE sparse matrix
    Aproj_stz = A.proj_stz, # mesh to prediction point projection matrix for stz
    Aproj_s = A.proj_s, # mesh to prediction point projection matrix for s
    Aproj_t = A.proj_t, # mesh to prediction point projection matrix for t
    Aproj_z = A.proj_z, # mesh to prediction point projection matrix for z
    ID_k = d$row_id_trans - 1, # observation ID
    lik_gaussian_i = lik_gaussian, # data likelihood for each row
    lik_binomial_i = lik_binomial, # data likelihood for each row
    sd_i = sd_i, # crossalked standard deviation
    stacker_col_id = stacker_col_id, # vector indicating which col of X_xp is a stacker & sum-to-1 (1) or not (0)
    options = list(
      use_priors = 1, # option1==1 use priors
      adreport_off = 1, # option2==1 ADREPORT off
      pixel_random = nugget, # option3==1 include nugget
      country_random = country_re, # option4==1 country random effects
      NID_random = nid_re, # option5==1 NID random effects
      main_space_effect = as.numeric(main_space_effect),
      main_time_effect = as.numeric(main_time_effect),
      main_age_effect = as.numeric(main_age_effect),
      full_interacting_effect = as.numeric(full_interacting_effect)
    ),
    prior_log_pixelre_sigma = read_inla_prior_sigma(nugget_prior),
    prior_log_cre_sigma = read_inla_prior_sigma(ctry_re_prior),
    prior_log_nidre_sigma = read_inla_prior_sigma(nid_re_prior),
    prior_matern = spde_list$prior,
    fconstraints = tmb_cov_constraint(colnames(X_xp), cov_constraints),
    aggweight_k = d$agg_weight
  )

  # Set staring values for parameters
  Parameters <- list(
    alpha_j = fe_start, # FE parameters alphas
    logtau = spde$param.inla$theta.mu[1], # Matern/AR tau
    logkappa = spde$param.inla$theta.mu[2], # Matern Range
    trho = 0.95, # temporal rho
    zrho = 0.95, # 3rd dimension of GP rho (TODO)
    log_pixelre_sigma = -1, # log(SD) of the normal nugget term
    log_cre_sigma = -1, # log(SD) of the normal country intercept term (later add slopes as vector)
    log_nidre_sigma = -1, # log(SD) of the normal NID intercept term
    log_gauss_sigma = -1, # log(SD) of normal model
    Epsilon_stz = Epsilon_stz, # Random Effects: GP locations
    Epsilon_s = Epsilon_s, # Random Effects: GP locations
    Epsilon_t = Epsilon_t, # Random Effects: GP locations
    Epsilon_z = Epsilon_z, # Random Effects: GP locations
    pixel_re = rep(0, max(pixel_re_vec) + 1), # Random Effects: Nugget Values
    cntry_re = rep(0, nrow(cntry_re_map)), # Random Effects Values of country random effects (later add in slope stuff)
    nid_re = rep(0, max(nid_re_vec) + 1)
  ) # Random Effects Values of nid random effects (later add in slope stuff)


  # put bounds on parameters (Note, this is especially important for rhos)
  L <- c(rep(-10, ncol(X_xp)), -10, -10, -99, -99, -10, -10, -10) ## updated the rho limits from .99999 since I transformed them in the cpp
  U <- c(rep(10, ncol(X_xp)), 10, 10, 99, 99, 10, 10, 10)
  L[which(stacker_col_id == 1)] <- -Inf
  U[which(stacker_col_id == 1)] <- Inf
  pn <- c(rep("alpha_j", ncol(X_xp)), "logtau", "logkappa", "trho", "zrho", "log_pixelre_sigma", "log_cre_sigma", "log_nidre_sigma")
  names(L) <- names(U) <- pn

  # return the list
  return(list(
    Data = Data,
    Parameters = Parameters,
    cs_df = cs_df,
    clamper = clamper,
    coords = coords,
    mesh = mesh_s,
    cntry_re_map = cntry_re_map,
    L = L,
    U = U
  ))
}
