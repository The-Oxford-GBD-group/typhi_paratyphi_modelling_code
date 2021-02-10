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
#' so that the output SDreport object has a sparse ordering? This option
#' requires the metis install for TMB.
#'
#' @return list of tmb model objects: ADfun, opt, sdrep
#'
#' @useDynLib mbg_tmb_model
#'
#' @export
#'
#' @note TODO support for other data models, sum to one constraint, country random effects
fit_mbg_tmb <- function(lbdcorerepo = core_repo,
                        cpp_template = "mbg_tmb_model",
                        tmb_input_stack = input_data,
                        ADmap_list = NULL,
                        control_list = NULL,
                        optimizer = "nlminb",
                        sparse_ordering = TRUE,
                        seed = NULL) {
  set.seed(seed)
  increment_seed(seed)

  # compile the cpp file and dynload it
  message("compiling template")
  TMB::compile(sprintf("%s/mbg_central/%s.cpp", lbdcorerepo, cpp_template))
  dyn.load(TMB::dynlib(sprintf("%s/mbg_central/%s", lbdcorerepo, cpp_template)))

  # deal with parallelization
  threads <- system("echo $OMP_NUM_THREADS", intern = TRUE)
  if (threads != "") {
    message(sprintf("Detected %s threads in OMP environmental variable.", threads))
    TMB::openmp(as.numeric(threads))
  } else {
    message("Did not detect environmental OMP variable, defaulting to 4 cores. 

             You can set this using OMP_NUM_THREADS or when launching singularity image.")
    TMB::openmp(4)
  }

  # set Data flag for Kaspers normalization fix
  tmb_input_stack$Data$flag <- 1
  # Initialize random effects
  randompars <- c()


  # Initialize Autodiff function map list (used to fix parameters)
  if (is.null(ADmap_list)) ADmap_list <- list()


  if (tmb_input_stack$Data$options$full_interacting_effect == 1) {
    randompars <- c(randompars, "Epsilon_stz")
  } else {
    ADmap_list[["Epsilon_stz"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$Epsilon_stz))
  }
  parlist <- NULL
  if (tmb_input_stack$Data$options$main_space_effect == 1) {
    randompars <- c(randompars, "Epsilon_s")
  } else {
    ADmap_list[["Epsilon_s"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$Epsilon_s))
    parlist <- c(parlist, "logtau", "logkappa")
  }
  if (tmb_input_stack$Data$options$main_time_effect == 1) {
    randompars <- c(randompars, "Epsilon_t")
  } else {
    ADmap_list[["Epsilon_t"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$Epsilon_t))
    parlist <- c(parlist, "trho")
  }
  if (tmb_input_stack$Data$options$main_age_effect == 1) {
    randompars <- c(randompars, "Epsilon_z")
  } else {
    ADmap_list[["Epsilon_z"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$Epsilon_z))
    parlist <- c(parlist, "zrho")
  }

  # If no z-col, set zrho to NA (If there is no z-col, Epsilon_stz is 2 dimensions)
  if (length(dim(tmb_input_stack$Parameters$Epsilon_stz)) == 2) {
    ADmap_list[["zrho"]] <- factor(NA)
  }
  # If no t-col, set trho to NA (Epsilon_stz will be 3 dimensions even if length(yl == 1))
  if (dim(tmb_input_stack$Parameters$Epsilon_stz)[2] == 1) {
    ADmap_list[["trho"]] <- factor(NA)
  }

  if (tmb_input_stack$Data$options$full_interacting_effect == 0) {
    for (par in parlist) {
      ADmap_list[[par]] <- factor(NA)
    }
  }

  if (tmb_input_stack$Data$options$pixel_random == 1) {
    # if pixel option is on add pixel to randompars
    randompars <- c(randompars, "pixel_re")
  } else {
    # if no pixel, add pixel related parameters to ignorelist
    ADmap_list[["log_pixelre_sigma"]] <- factor(NA)
    ADmap_list[["pixel_re"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$pixel_re))
  }

  if (tmb_input_stack$Data$options$country_random == 1) {
    # if country RE option is on add cntry_re to randompars
    randompars <- c(randompars, "cntry_re")
  } else {
    # if no country re, add cntry_re related parameters to ignorelist
    ADmap_list[["log_cre_sigma"]] <- factor(NA)
    ADmap_list[["cntry_re"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$cntry_re))
  }

  if (tmb_input_stack$Data$options$NID_random == 1) {
    # if NID RE option is on add nid_re to randompars
    randompars <- c(randompars, "nid_re")
  } else {
    # if no NID RE, add nid_re related parameters to ignorelist
    ADmap_list[["log_nidre_sigma"]] <- factor(NA)
    ADmap_list[["nid_re"]] <- rep(factor(NA), length(tmb_input_stack$Parameters$nid_re))
  }

  if (sum(tmb_input_stack$Data$lik_gaussian_i) == 0) {
    # map out model sigma if no gaussian observations
    ADmap_list[["log_gauss_sigma"]] <- factor(NA)
  }

  # Print fixed parameters and random parameters to be used in TMB run
  message(paste0("ADMAP_LIST: ", paste0(names(ADmap_list), collapse = ", ")))
  message(paste0("Random Pars: ", paste0(randompars, collapse = ", ")))

  # If all of the random effects are used, set ADmap_list back to NULL
  if (length(ADmap_list) == 0) ADmap_list <- NULL

  # make the AD object
  message("Making AD object")
  obj <- TMB::MakeADFun(
    data = tmb_input_stack$Data,
    parameters = tmb_input_stack$Parameters,
    map = ADmap_list,
    random = randompars,
    hessian = TRUE,
    DLL = cpp_template
  )

  # normalize
  obj <- TMB::normalize(obj, flag = "flag")

  # Reduce fill in of sparse Cholesky factorization (requires metis install of TMB)
  if (sparse_ordering) {
    TMB::runSymbolicAnalysis(obj)
  }
  message(obj)
  message(obj$par)
  message(control_list)
  # Run optimizer
  message("Running MLE")
  if (optimizer == "nlminb") {
    opt0 <- do.call("nlminb", list(
      start = obj$par,
      objective = obj$fn,
      gradient = obj$gr,
      lower = tmb_input_stack$L,
      upper = tmb_input_stack$U,
      control = control_list
    ))
  }
  if (optimizer == "optim") {
    opt0 <- do.call("optim", list(par = obj$par, fn = obj$fn, control = control_list, gr = obj$gr, method = "BFGS"))
  }

  # run sdreport to get joint precision of all parameters
  for (i in 1:20) message("Getting Joint Precision")
  SD0 <- TMB::sdreport(obj, getJointPrecision = TRUE, bias.correct = TRUE)

  # return
  return(list(
    ADfun = obj,
    opt = opt0,
    sdrep = SD0,
    fenames = colnames(tmb_input_stack$Data$X_kj)
  ))
}
