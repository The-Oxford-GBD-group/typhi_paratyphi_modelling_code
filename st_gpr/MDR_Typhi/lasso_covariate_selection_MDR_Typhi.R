library(glmnet)
library(data.table)
rm(list = ls())

#Test out the covariates using Lasso as covariate selection

#i. Specify the data you are using ----

#Load data
mydata <- data.table(read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/MDR_typhi_outliered.csv', stringsAsFactors = F))

#Load covariates
covs <- read.csv('/ihme/homes/annieb6/covariates/cleaned_covs.csv', stringsAsFactors = F)

#specify the family you are modelling (currently can use binomial or gaussian)
family <- 'binomial'

#specify transformation of the data to do - 'logit', 'log' or NULL
transformation <- 'logit'

#specify what you columns are
p <- 'val'                      #the proportion of your indicator successes
n <- NULL            #the number of your indicator successes
d <- 'sample_size'                     #the denoinator (sample size)
w <- NULL                      #the weights to use

covs_to_include <-  c("cv_anc1_coverage_prop",
                      "cv_anc4_coverage_prop",            
                      "cv_dtp3_coverage_prop",         
                      "cv_hib3_coverage_prop",
                      "cv_hospital_beds_per1000",
                      "cv_latitude",                      
                      "cv_ldi_pc", 
                      "cv_mean_temperature",              
                      "cv_pollution_outdoor_pm25",
                      "cv_sanitation_prop",               
                      "cv_sba_coverage_prop",
                      "cv_tfr",                           
                      "cv_water_prop",
                      "cv_maternal_educ_yrs_pc",          
                      "cv_prop_urban", 
                      "cv_stunting_prop_haz_under_2sd",
                      "cv_he_cap" ,                       
                      "cv_ors", 
                      "cv_antimalarial_effective_tmt_map",
                      "cv_handwashing",
                      "cv_zinc_treatment",
                      "cv_physicians_pc", 
                      "cv_pharmacists_pc",
                      "cv_hiv",
                      "cv_lri",
                      "cv_diabetes",
                      'cv_abx_prop', 
                      'cv_haqi',
                      'cv_sdi',
                      'QA',
                      "ddd_per_1000",
                      "J01B",                             
                      "J01C",
                      "J01E",
                      "J01M",
                      "cv_intest_typhoid",
                      "cv_intest_paratyph",
                      "cv_cvd",
                      "cv_resp",
                      "cv_neuro")

#~~~~~~~~~~~~~~~~~~#
# Run the lasso ####
#~~~~~~~~~~~~~~~~~~#
#1. Data set up
#rename some colums to avoid confusion
colnames(mydata)[colnames(mydata)==d] <- 'd' 
if(!is.null(p)) {colnames(mydata)[colnames(mydata)==p] <- 'p'} 
if(!is.null(n)) {colnames(mydata)[colnames(mydata)==n] <- 'n'} 

#if you dont have n but have p and d
if(is.null(n) &!is.null(p)&!is.null(d)){mydata$n <- mydata$p*mydata$d}

#if you havent specified a weights column set to 1
if(is.null(w)){
  mydata$w <- 1
} else {
  colnames(mydata)[colnames(mydata)==w] <- 'w' 
}

#perform transformations as specified
if(is.null(transformation)){
} else if(transformation == 'log'){
  if(family == 'binomial'){
    mydata$n <- log(mydata$n)
    mydata$d <- log(mydata$d)
    mydata$p <- log(mydata$p)
  } else if(family == 'gaussian')
    mydata$n <- log(mydata$n)
} else if(transformation == 'logit'){  #ln(p/1-p)
  if(family == 'binomial'){
    mydata$p <- log(mydata$p/(1-mydata$p))
  } else if(family == 'gaussian'){
    message('should not be using logit transformation with gaussian data')
  }
}

## shuffle the data into five random folds
mydata <- mydata[sample(nrow(mydata)),]
mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]

## add a row id column
mydata[, a_rowid := seq(1:nrow(mydata))]

#restrict to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']
covs <- data.table(covs)

#merge covs onto data
mydata <- merge(mydata, covs)
mydata <- data.table(mydata)

## remove NAs
if(family == 'binomial'){
  mydata    <- na.omit(mydata, c('n', 'd', 'p', names(covs)))
}

if(family == 'gaussian'){
  mydata    <- na.omit(mydata, c('n', names(covs)))
}

#2. Run the lasso 
#define what your response variable is in the data
if(family == 'binomial'){
  response <- cbind(failures   = mydata$d - mydata$n, 
                    successes = mydata$n)
}else if(family == 'gaussian'){
  response <- mydata$n
}

#define variables to include (as a matrix)
vars <- as.matrix(mydata[, covs_to_include, with = F])
colnames(vars) <- covs_to_include

#fit cross validated lasso to select lambda
cv_lasso = cv.glmnet(x = vars , y= response, family = family, alpha = 1, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)

#print out the model coefficients.
cv_lasso$lambda.1se
coef(cv_lasso, s = "lambda.1se")

#trial other lambdas to get approx 10 most influential covariates
coef(cv_lasso, s = 0.05)


#~~~~~~~~~~~~~~~~~~~~~#
#       END           #
#~~~~~~~~~~~~~~~~~~~~~#