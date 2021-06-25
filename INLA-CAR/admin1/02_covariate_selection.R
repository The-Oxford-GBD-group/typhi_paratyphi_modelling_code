library(data.table)
library(ggplot2)
library(sf)
library(foreign)
library(glmnet)
rm(list = ls())

# Annual covariates first ####
annual.covs <- fread('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/annual_covs_adm1.csv')

annual.covs <- annual.covs[order(admin_code, year)]
annual.covs <- annual.covs[!is.na(annual.covs$admin_code)]
annual.covs <- annual.covs[!is.na(annual.covs$year)]

# Fill in the years missing data (define function and apply tp all covariates)
impute.missing.years <- function(X, year_vector = annual.covs$year, all_years = 1990:2019){
  # Find the missing years 
  complete_years <- aggregate(X, by=list(year_vector), FUN=sum, na.rm=TRUE)  
  complete_years <- complete_years$Group.1[complete_years[,2] != 0]
  # Match missing years to the nearest year with data
  dt = data.table(complete_years, val = complete_years) 
  setattr(dt, "sorted", "complete_years")
  setkey(dt, complete_years) 
  matched_years <- dt[J(all_years), roll = "nearest"]
  matched_years <- matched_years[matched_years$complete_years!=matched_years$val,]
  
  # Use the values for the closest year to fill in the gaps
  if(nrow(matched_years)>0){
    for(i in 1:nrow(matched_years)){
      X[year_vector == matched_years$complete_years[i]] <- 
        X[year_vector == matched_years$val[i]]
    }
  }
  return(X)
}

annual.covs[,3:18] <- data.frame(apply(annual.covs[,3:18],2,impute.missing.years))

# Synoptic covariates ####
synopic_covs <- fread('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/synoptic_covs_adm1.csv')
synopic_covs <- synopic_covs[!is.na(synopic_covs$admin_code)]

# Add national covariates ####
# (need to add a ISO3 code to this and the other covs to merge)
national_covs <- fread("Z:/AMR/Covariates/modelling_covariates/national_covs_imputed_missing.csv")
locs <- read.dbf('C:/Users/annieb/Desktop/GBD2020_analysis_final.dbf')
locs <- locs[c('loc_id', 'ihme_lc_id')]
locs$ihme_lc_id <-  as.character(locs$ihme_lc_id)
national_covs <- merge(national_covs, locs, by.x = 'location_id', by.y = 'loc_id')
names(national_covs)
national_covs <- national_covs[,.(ihme_lc_id, year_id,
                                  vae,           
                                  pve,
                                  gee,
                                  rqe,
                                  rle,                           
                                  cce,
                                  universal_health_coverage, 
                                  he_cap,
                                  frac_oop_hexp,
                                  J01A,  
                                  J01B,  
                                  J01C,  
                                  J01D,
                                  J01E,
                                  J01F,                           
                                  J01G,
                                  J01M,
                                  ddd_per_1000, 
                                  intest_typhoid, 
                                  physicians_pc, 
                                  pharmacists_pc, 
                                  hospital_beds_per1000,
                                  anc1_coverage_prop,
                                  anc4_coverage_prop,
                                  dtp3_coverage_prop,
                                  maternal_educ_yrs_pc,
                                  sanitation_prop_nat = sanitation_prop,
                                  stunting_prop_haz_under_2sd,
                                  water_prop_nat = water_prop,
                                  hib3_coverage_prop,
                                  antimalarial_effective_tmt_map)]

# add 2019 onto national covs (just use 2018 for now, will update this later)
ad_yr <- national_covs[national_covs$year_id == 2018]
ad_yr$year_id <- 2019
national_covs <- rbind(national_covs, ad_yr)
rm(ad_yr)

# Merge covariates together ####
all_covs <- merge(annual.covs, synopic_covs, by = 'admin_code', all.x = T, all.y = T, allow.cartesian = T)

#get the country onto the admin 1 covariates
admin1 <- read.dbf('C:/users/annieb/desktop/admin2013_1.dbf')
admin1 <- admin1[c('GAUL_CODE', 'COUNTRY_ID')] 
admin1$COUNTRY_ID <-  as.character(admin1$COUNTRY_ID)
all_covs <- merge(all_covs, admin1, by.x = 'admin_code', by.y = 'GAUL_CODE')

all_covs <-  merge(all_covs, national_covs, by.x = c('COUNTRY_ID', 'year'), by.y = c('ihme_lc_id', 'year_id'), allow.catesian = T, all.x = T)

#for covariates with only partial subnational coverage use the national values for blank locations
all_covs$dpt3_cov[is.na(all_covs$dpt3_cov)] <- all_covs$dtp3_coverage_prop[is.na(all_covs$dpt3_cov)]
all_covs$edu_mean[is.na(all_covs$edu_mean)] <- all_covs$maternal_educ_yrs_pc[is.na(all_covs$edu_mean)]
all_covs$sanitation_prop[is.na(all_covs$sanitation_prop)] <- all_covs$sanitation_prop_nat[is.na(all_covs$sanitation_prop)]
all_covs$water_prop[is.na(all_covs$water_prop)] <- all_covs$water_prop_nat[is.na(all_covs$water_prop)]
all_covs$stunting_mod_b[is.na(all_covs$stunting_mod_b)] <- all_covs$stunting_prop_haz_under_2sd[is.na(all_covs$stunting_mod_b)]
all_covs$hib3_cov[is.na(all_covs$hib3_cov)] <- all_covs$hib3_coverage_prop[is.na(all_covs$hib3_cov)]

all_covs$dtp3_coverage_prop <-  NULL
all_covs$maternal_educ_yrs_pc <-  NULL
all_covs$sanitation_prop_nat <-  NULL
all_covs$water_prop_nat <-  NULL
all_covs$stunting_prop_haz_under_2sd <-  NULL
all_covs$hib3_coverage_prop <-  NULL

#remove the subnat antimalarial treatment cov as is only for africa and cannot find clear definition of what this is do remove
all_covs$map_antimalarial <-  NULL

#save covs
write.csv(all_covs, 'Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv', row.names = F)

#Plot out maps of covariates
admin1_shp <- st_read('C:/users/annieb/desktop/admin2013_1.shp')
covs.map <- merge(admin1_shp, all_covs, by.x = 'GAUL_CODE', by.y = 'admin_code')

for(i in 1:length(names(all_covs)[3:49])){
  plot_data <- covs.map
  colnames(plot_data)[6+i] <- 'plot_cov'
  plot_data <- na.omit(plot_data[c('year', 'plot_cov', 'geometry')])
  png(paste0('C:/Users/annieb/Desktop/cov_plots/', names(all_covs)[3:49][i], '.png'),
      height = 40, width = 30, units = 'cm', res = 200)
  print(
    ggplot(plot_data)+
      geom_sf(aes(fill = plot_cov), size = 0.01, colour = 'black')+
      theme_bw()+
      theme(line = element_blank(),
            axis.text = element_blank())+
      scale_fill_viridis(option='inferno', discrete = F, direction = -1)+
      facet_wrap(~year)+
      xlim(-20,150)+
      ylim(-35,55)
  )
  dev.off()
}

# Associations between data and covs #####
# merge covariates onto the data
rm(list = ls())
mydata <- fread('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/outliered/FQNS_Paratyphi_outliered.csv')
all_covs <- fread('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv')
names(mydata)[names(mydata)=='year_id'] <- 'year'
names(all_covs)[names(mydata)=="COUNTRY_ID"] <- 'country'
mydata <- mydata[is_outlier ==0,]
mydata <- merge(mydata, all_covs)
rm(all_covs)


# reshape long
long_covs <- melt(data = mydata,
                  id.vars = c('adj_id', 'year', 'val', 'super_region'),
                  measure.vars = 24:69,
                  variable.name = 'covariate',
                  value.name = 'cov_val')


#create table of correlations between data and covariates
correlations <- long_covs[,.(r = round(cor(val[!is.na(val)&!is.na(cov_val)], cov_val[!is.na(val)&!is.na(cov_val)]),4)),
                          by = c('covariate')]

correlations$direction <- NA
correlations$direction[correlations$r <0] <- "-"
correlations$direction[correlations$r >0] <- "+"
correlations$r2 <- correlations$r^2
correlations$r <- NULL
write.csv(correlations, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Paratyphi_correlations.csv', row.names = F)

#create table of correlations between data and covariates
correlations <- long_covs[,.(r = round(cor(val[!is.na(val)&!is.na(cov_val) & super_region == 'Sub-Saharan Africa'], cov_val[!is.na(val)&!is.na(cov_val)& super_region == 'Sub-Saharan Africa']),4)),
                          by = c('covariate')]

correlations$direction <- NA
correlations$direction[correlations$r <0] <- "-"
correlations$direction[correlations$r >0] <- "+"
correlations$r2 <- correlations$r^2
correlations$r <- NULL
write.csv(correlations, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Typhi_correlations_Africa.csv', row.names = F)

correlations <- long_covs[,.(r = round(cor(val[!is.na(val)&!is.na(cov_val) & super_region != 'Sub-Saharan Africa'], cov_val[!is.na(val)&!is.na(cov_val)& super_region != 'Sub-Saharan Africa']),4)),
                          by = c('covariate')]

correlations$direction <- NA
correlations$direction[correlations$r <0] <- "-"
correlations$direction[correlations$r >0] <- "+"
correlations$r2 <- correlations$r^2
correlations$r <- NULL
write.csv(correlations, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Typhi_correlations_Asia.csv', row.names = F)

# plot out covs
#1. Standard covs vs standard data
png("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Paratyphi_vs_covs.png",
    height = 30,
    width = 30, units = 'cm', res = 150)
ggplot(long_covs, aes(x = val, y = cov_val))+
  geom_point()+
  facet_wrap(~covariate, ncol = 5, scales = 'free')+
  labs(x = 'Proportion resistant', y = 'Covariate value')
dev.off() 

#plot for Africa and Asia seperately
png("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Typhi_vs_covs_Africa.png",
    height = 30,
    width = 30, units = 'cm', res = 150)
ggplot(long_covs[long_covs$super_region == 'Sub-Saharan Africa',], aes(x = val, y = cov_val))+
  geom_point()+
  facet_wrap(~covariate, ncol = 5, scales = 'free')+
  labs(x = 'Proportion resistant', y = 'Covariate value')
dev.off() 

png("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/covariate_selection/FQNS_Typhi_vs_covs_asia.png",
    height = 30,
    width = 30, units = 'cm', res = 150)
ggplot(long_covs[long_covs$super_region != 'Sub-Saharan Africa',], aes(x = val, y = cov_val))+
  geom_point()+
  facet_wrap(~covariate, ncol = 5, scales = 'free')+
  labs(x = 'Proportion resistant', y = 'Covariate value')
dev.off() 


rm(long_covs, correlations)

# Lasso covariate selection
## shuffle the data into five random folds
mydata <- mydata[sample(nrow(mydata)),]
mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]

## add a row id column
mydata[, a_rowid := seq(1:nrow(mydata))]

response <- cbind(failures   = mydata$sample_size - mydata$number_resistant, 
                  successes = mydata$number_resistant)

#define variables to include (as a matrix)
vars <- as.matrix(scale(mydata[, c(23:35, 37:69)]))
colnames(vars) <- names(mydata[, c(23:35, 37:69)])

mydata$w <- 1

#fit cross validated lasso to select lambda
cv_lasso = cv.glmnet(x = vars , y= response, family = 'binomial', alpha = 1, weights = mydata$w, nfolds = 5, foldid = mydata$fold_id)

#print out the model coefficients.
cv_lasso$lambda.1se
coef(cv_lasso, s = "lambda.1se")

#trial other lambdas to get approx 10 most influential covariates
coef(cv_lasso, s = 0.005)

# Just for Africa
#define variables to include (as a matrix)
response <- cbind(failures   = mydata$d[mydata$super_region == 'Sub-Saharan Africa'] - mydata$n[mydata$super_region == 'Sub-Saharan Africa'], 
                  successes = mydata$n[mydata$super_region == 'Sub-Saharan Africa'])


vars <- as.matrix(scale(mydata[super_region == 'Sub-Saharan Africa', 12:56]))
colnames(vars) <- names(mydata[, 12:56])

#fit cross validated lasso to select lambda
cv_lasso = cv.glmnet(x = vars , y= response, family = 'binomial', alpha = 1, weights = mydata$w[mydata$super_region=='Sub-Saharan Africa'], nfolds = 5, foldid = mydata$fold_id[mydata$super_region=='Sub-Saharan Africa'])

#print out the model coefficients.
cv_lasso$lambda.1se
coef(cv_lasso, s = "lambda.1se")
coef(cv_lasso, s = 0.008)

#and for Asia
response <- cbind(failures   = mydata$d[mydata$super_region != 'Sub-Saharan Africa'] - mydata$n[mydata$super_region != 'Sub-Saharan Africa'], 
                  successes = mydata$n[mydata$super_region != 'Sub-Saharan Africa'])

#define variables to include (as a matrix)
vars <- as.matrix(scale(mydata[super_region != 'Sub-Saharan Africa', c(12:24, 26:56)]))
colnames(vars) <- names(mydata[, c(12:24, 26:56)])

mydata$w <- 1

#fit cross validated lasso to select lambda
cv_lasso = cv.glmnet(x = vars , y= response, family = 'binomial', alpha = 1, weights = mydata$w[mydata$super_region != 'Sub-Saharan Africa'], nfolds = 5, foldid = mydata$fold_id[mydata$super_region != 'Sub-Saharan Africa'])

#print out the model coefficients.
cv_lasso$lambda.1se
coef(cv_lasso, s = "lambda.1se")
coef(cv_lasso, s = 0.02)

