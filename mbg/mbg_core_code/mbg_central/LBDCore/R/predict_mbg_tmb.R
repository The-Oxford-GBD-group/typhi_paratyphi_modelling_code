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
#' corresponding to column names in d
#' @param sr simple raster object
#' @param yl a vector of years for analysis (i.e. c(2001,2002,2003))
#' @param zl a vector of z for analysis (i.e. c(1,2,3,4,5)). Config item z_list. No z-dimension it should just be zero
#' @param covs_list named list of covariate bricks, each should be length(yl) long
#' @param clamp_covs Should covariates be 'clamped' to not predict outside of
#' their observed range in the data?
#' @param cov_constraints named int vector. integer vector indexed by covariate
#' names, in the format returned by the function
#' \code{\link{covariate_constraint_vectorize}}. NOTE: In the current
#' implementation, priors for fixed effects are set to the same values
#' regardless of whether the covariate fixed effect is constrained or not.
#' Apply constraints with caution.
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
                            seed = NULL,
                            tmb_input_stack = input_data,
                            model_fit_object = model_fit,
                            fes = all_fixed_effects,
                            sr = simple_raster,
                            yl = year_list,
                            zl = z_list,
                            transform = "inverse-logit",
                            covs_list = cov_list,
                            clamp_covs = FALSE,
                            cov_constraints = use_global_if_missing("cov_constraints"),
                            use_full_interacting_effect = as.logical(use_gp),
                            use_space_only_gp = as.logical(use_space_only_gp),
                            use_time_only_gmrf = as.logical(use_time_only_gmrf),
                            use_age_only_gmrf = as.logical(use_age_only_gmrf),
                            coefs.sum1 = coefs_sum1) {

  # Pull SD report object
  sdrep <- model_fit_object$sdrep

  # pull a few useful things from the input data stack
  cs_transform <- tmb_input_stack$cs_df
  mesh <- tmb_input_stack$mesh
  cntry_re_map <- tmb_input_stack$cntry_re_map
  if (clamp_covs == TRUE) {
    clamper <- tmb_input_stack$clamper
  } else {
    clamper <- NULL
  }
  stacker_col_id <- tmb_input_stack$Data$stacker_col_id

  # set seed if it is requested
  if (!is.null(seed)) {
    set.seed(seed)
    increment_seed(seed)
  }
  # vector of means
  mu <- c(sdrep$par.fixed, sdrep$par.random)

  # simulate draws
  draws <- rmvnorm_prec(mu = mu, prec = sdrep$jointPrecision, n.sims = samples)

  ## separate out the draws
  parnames <- c(names(sdrep$par.fixed), names(sdrep$par.random))

  if (use_full_interacting_effect == T) {
    epsilon_stz_draws <- draws[parnames == "Epsilon_stz", ]
  }
  if (use_space_only_gp == T) {
    epsilon_s_draws <- draws[parnames == "Epsilon_s", ]
  }
  if (use_time_only_gmrf == T) {
    epsilon_t_draws <- draws[parnames == "Epsilon_t", ]
  }
  if (use_age_only_gmrf == T) {
    epsilon_z_draws <- draws[parnames == "Epsilon_z", ]
  }

  alpha_draws <- draws[parnames == "alpha_j", ]

  # seperate out Z FE draws
  FE_z_draws <- NULL
  if (length(zl) > 1) {
    FE_z_draws <- alpha_draws[ grepl("FE_z_level__", model_fit_object$fenames), ] # separate out z-level fixed effects from other FEs
    alpha_draws <- alpha_draws[!grepl("FE_z_level__", model_fit_object$fenames), ] # remove any z-level fixed effects from other FEs
  }


  if (length(zl) > 1) {
    if (dim(FE_z_draws)[1] != (length(zl) - 1)) {
      stop("Incorrect number of fixed effects for levels of z in the z_list")
    }
  }

  # names of fes
  tmb_const <- tmb_cov_constraint(model_fit_object$fenames, cov_constraints)
  fes <- unlist(strsplit(fes, " \\+ "))

  # mask covariates to simple raster
  for (l in 1:length(covs_list)) {
    covs_list[[l]] <- crop(covs_list[[l]], extent(sr))
    covs_list[[l]] <- setExtent(covs_list[[l]], sr)
    covs_list[[l]] <- mask(covs_list[[l]], sr)
  }

  # keep only covariates used in model, typically stacking
  covs_list <- covs_list[names(covs_list) %in% fes]

  # get coordinates of full projection space
  # Extract admin0 code
  f_orig <- data.table(cbind(xyFromCell(sr, seegSDM:::notMissingIdx(sr)), adm_code = as.vector(sr[seegSDM:::notMissingIdx(sr)])))
  f_orig$t <- f_orig$z <- 1 # set initial time and Z
  f_orig[, tmpord := 1:.N]

  # use the country code dt from input_data to map admin0 code to RE values
  f_orig <- merge(f_orig, cntry_re_map[, c("adm_code", "re_id"), with = FALSE], by = "adm_code", all.x = TRUE)
  f_orig <- f_orig[order(tmpord)] # make 100% sure everything is correctly ordered after the merge.
  f_orig[, re_id := re_id + 1 ] # to deal with indexing which started at 0 in the cpp
  f_orig$re_id[is.na(f_orig$re_id)] <- 0 # to deal with countries not in the data

  # add time periods and z periods as needed
  grp <- setDT(expand.grid(1:length(yl), 1:length(zl)))
  setnames(grp, c("Var1", "Var2"), c("t", "z"))
  grp[, group := 1:.N]
  fullsamplespace <- data.table()
  for (g in 1:max(grp$group)) {
    tmp <- f_orig
    tmp[, z := grp$z[grp$group == g]]
    tmp[, t := grp$t[grp$group == g]]
    tmp[, gp := g]
    fullsamplespace <- rbind(fullsamplespace, tmp)
  }
  fullsamplespace[, idx := 1:.N]

  # pull out covariates in format we expect them
  # a list of length periods with a brick of named covariates inside
  new_cl <- list()
  if (length(covs_list) == 0) {
    message("No covariates detected, predicting using intercept only.")
  } else {
    message(sprintf("%i covariates detected.", length(covs_list)))

    for (p in 1:length(yl)) {
      new_cl[[p]] <- list()
      for (n in names(covs_list)) {
        if (dim(covs_list[[n]])[3] == 1) { # synoptic mean covariates
          new_cl[[p]][[n]] <- covs_list[[n]]
        } else if (dim(covs_list[[n]])[3] == length(yl)) { # time varying covariates
          new_cl[[p]][[n]] <- covs_list[[n]][[p]]
        } else { # error if there is some other weird non-conforming year thing
          stop(sprintf(
            "Covariate %n is a brick with %i layers, while year_list has %i years",
            n, dim(covs_list[[n]])[3], length(yl)
          ))
        }
      }
      new_cl[[p]] <- brick(new_cl[[p]])
    }
  }

  # get surface locs to project on to
  pcoords <- cbind(x = fullsamplespace$x, y = fullsamplespace$y) ## used for cov raster extract

  ## setup coords for GP projection. convert coords to spherical if
  ## using spherical modeling mesh. used if you made a mesh on s2
  if (mesh_s$manifold == "S2") {
    gp_coords <- lonlat3D(pcoords[, 1], pcoords[, 2])
  } else {
    gp_coords <- pcoords
  }

  ## define grouping across periods
  groups_periods <- fullsamplespace$gp

  # extract cell values  from covariates, deal with timevarying covariates here
  cov_vals <- list()
  for (z in 1:length(zl)) {
    cov_vals[[z]] <- list()
    for (p in 1:length(yl)) {
      if (length(fes) > 0) {

        # raster extract and keep only fes
        cov_vals[[z]][[p]] <- raster::extract(new_cl[[p]], pcoords[1:nrow(f_orig), ])
        cov_vals[[z]][[p]] <- cov_vals[[z]][[p]][, colnames(cov_vals[[z]][[p]]) %in% c(fes)]

        # If there is only a single covariate, convert from vector to matrix
        if ((length(covs_list) == 1) & !("matrix" %in% class(cov_vals[[z]][[p]]))) {
          cov_vals[[z]][[p]] <- matrix(cov_vals[[z]][[p]], ncol = 1)
        }

        # transform raw covariate values (center scaled) if needed (i.e. if cs_tranform is not 1 0 for that variable)
        cov_vals[[z]][[p]] <- centreScale(cov_vals[[z]][[p]], cs_transform)

        # clamp covariates if clamper is not null
        if (!is.null(clamper)) {
          # message('Clamping')
          for (fe in fes) {
            tmpvec <- cov_vals[[z]][[p]][, colnames(cov_vals[[z]][[p]]) == fe]
            mn <- as.numeric(clamper[, fe, with = FALSE][1])
            mx <- as.numeric(clamper[, fe, with = FALSE][2])
            tmpvec[tmpvec < mn] <- mn
            tmpvec[tmpvec > mx] <- mx
            cov_vals[[z]][[p]][, colnames(cov_vals[[z]][[p]]) == fe] <- tmpvec
          }
        }
        # add an intercept
        cov_vals[[z]][[p]] <- cbind(int = 1, cov_vals[[z]][[p]])
      } else {
        # if no covariates just do intercept only
        cov_vals[[z]][[p]] <- cbind(int = rep(1, nrow(f_orig)))
      }
      # if there is a z column, add on those fixed effects indicators
      if (length(zl) > 1) {
        tmpzmat <- matrix(0, ncol = (length(zl) - 1), nrow = nrow(f_orig))
        colnames(tmpzmat) <- paste0("FE_z_level__", 2:length(zl))
        for (zz in 2:length(zl)) {
          if (z == zz) {
            tmpzmat[, paste0("FE_z_level__", zz)] <- 1
          }
        }
        cov_vals[[z]][[p]] <- cbind(cov_vals[[z]][[p]], tmpzmat)
      }
    }
  }

  # covariate values by alpha draws
  l_vals <- list()
  for (z in 1:length(zl)) {
    l_vals[[z]] <- list()
    for (p in 1:length(yl)) {
      # First apply constraints and then sum-to-one (note: if using sum-to-one do not also apply other constraints to those parameters)
      l_vals[[z]][[p]] <- cov_vals[[z]][[p]] %*% apply_sum_to_one(stacker_col_id, apply_constraints(tmb_const, rbind(alpha_draws, FE_z_draws)))
    }
  }
  cell_l <- do.call("rbind", unlist(l_vals, recursive = FALSE))

  ## use inla helper functions to project the spatial effect.
  A.pred_stz <- inla.spde.make.A(
    mesh = mesh_s,
    loc = gp_coords,
    group = groups_periods
  )

  # make a projection matrix from data to s mesh
  A.pred_s <- inla.spde.make.A(
    mesh = mesh_s,
    loc = gp_coords
  )

  ### values of GP ST surface at each cell (long by nperiods)
  # if we have multiple zs then do this by z since its possible to throw a SuiteSparse 'Problem too large' error here.
  if (use_full_interacting_effect) {
    if (length(zl) > 1) {
      cell_stz <- list()
      for (zz in 1:length(zl)) {
        cell_stz[[zz]] <- as.matrix(A.pred_stz[(which(fullsamplespace$z == zz)), ] %*% epsilon_stz_draws)
      }
      cell_stz <- do.call("rbind", cell_stz)
    } else {
      cell_stz <- as.matrix(A.pred_stz %*% epsilon_stz_draws)
    }
  }

  ### values of GP S surface at each cell (long by nperiods)
  if (use_space_only_gp) {
    if (length(zl) > 1) {
      cell_s <- list()
      for (zz in 1:length(zl)) {
        cell_s[[zz]] <- as.matrix(A.pred_s[(which(fullsamplespace$z == zz)), ] %*% epsilon_s_draws)
      }
      cell_s <- do.call("rbind", cell_s)
    } else {
      cell_s <- as.matrix(A.pred_s %*% epsilon_s_draws)
    }
  }

  # Get raster & raster*year vectors for use in GPT & GPZ surface creation
  cell_idx <- seegSDM:::notMissingIdx(sr)
  s.vec <- 1:length(sr[cell_idx]) ## vector of spatial cell count
  st.vec <- rep(s.vec, length(yl)) ## expand s vec in time

  ### values of GP T surface at each cell, across agebins
  if (use_time_only_gmrf) {
    cell_t <- list()
    for (z in 1:length(zl)) {
      cell_t[[z]] <- list()
      for (t in 1:length(yl)) {
        cell_t[[z]][[t]] <- sapply(
          epsilon_t_draws[t, ],
          function(it) rep(it, length(s.vec))
        )
      }
      cell_t[[z]] <- do.call("rbind", cell_t[[z]])
    }
    cell_t <- do.call("rbind", cell_t)
  }

  ### values of GP Z surface at each cell
  if (use_age_only_gmrf) {
    cell_z <- list()
    for (z in 1:length(zl)) {
      cell_z[[z]] <- list()
      for (t in 1:length(yl)) {
        cell_z[[z]][[t]] <- sapply(
          epsilon_z_draws[z, ],
          function(it) rep(it, length(s.vec))
        )
      }
      cell_z[[z]] <- do.call("rbind", cell_z[[z]])
    }
    cell_z <- do.call("rbind", cell_z)
  }
  # add the pixel-level re if needed (i.e. pixel-level re parameter was not mapped out)

  cell_nug <- matrix(0L, nrow = dim(cell_l)[1], ncol = dim(cell_l)[2])
  if ("log_pixelre_sigma" %in% parnames) {
    if (exists("no_nugget_predict") & no_nugget_predict == TRUE) {
      message("Pixel-level random effect not included in predict")
    } else {
      message("Adding pixel-level random effect")
      for (s in 1:samples) {
        cell_nug[, s] <- rnorm(dim(cell_nug)[1], 0, exp(draws[parnames == "log_pixelre_sigma", ])[s])
      }
    }
  }

  # add the country random intercept if it was estimated in the model
  cell_cre_int <- matrix(0L, nrow = dim(cell_l)[1], ncol = dim(cell_l)[2])
  if ("log_cre_sigma" %in% parnames) {
    message("adding country random intercept")
    cre <- data.table(draws[parnames == "cntry_re", ])
    cre[, re_id := 1:.N]
    cre <- merge(fullsamplespace, cre, by = "re_id", all.x = TRUE)
    cre <- cre[order(idx)]
    cre <- cre[, grep("V", colnames(cre)), with = FALSE]
    if (all(dim(cell_cre_int) == dim(cre))) {
      cell_cre_int <- as.matrix(cre)
      rm(cre)
    } else {
      stop("CHECK COUNTRY RE DIMENSIONS")
    }
  }

  # add together linear and st components
  pred_tmb <- cell_l + cell_nug + cell_cre_int

  if (use_full_interacting_effect) pred_tmb <- pred_tmb + cell_stz
  if (use_space_only_gp) pred_tmb <- pred_tmb + cell_s
  if (use_time_only_gmrf) pred_tmb <- pred_tmb + cell_t
  if (use_age_only_gmrf) pred_tmb <- pred_tmb + cell_z

  # transform
  if (transform == "inverse-logit") {
    pred_tmb <- plogis(as.matrix(pred_tmb))
  } else {
    pred_tmb <- eval(parse(text = sprintf("%s(as.matrix(pred_tmb))", transform)))
  }

  # if there is more than one z, then return a list of length zl cell_preds
  if (length(zl) > 1) {
    pred_tmb_list <- list()
    chunklength <- dim(pred_tmb)[1] / length(zl)
    for (z in 1:length(zl)) {
      pred_tmb_list[[z]] <- pred_tmb[((z - 1) * chunklength + 1):(chunklength * z), 1:samples]
    }
    pred_tmb <- pred_tmb_list
  }

  # return the predicted cell_pred object
  return(pred_tmb)
}
