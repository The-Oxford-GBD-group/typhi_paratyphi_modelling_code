# Outlier detection ####
rm(list = ls())

library(lme4)
library(foreign)
library(ggplot2)
library(ggforce)

# Setup ####
indicator = 'FQNS_typhi'
mydata <- read.csv(paste0('/share/homes/annieb6/AMR/typhi_paratyphi/datasets/', indicator, '.csv'), stringsAsFactors = F)
mydata <- mydata[mydata$sample_size>=5,]
mydata <- mydata[mydata$nid != 3286,]
if(grep('paratyphi', indicator) == 1){
  mydata <- mydata[mydata$region == 'South Asia' |
                     mydata$region == 'Southeast Asia' |
                     mydata$region == 'East Asia' |
                     mydata$region == 'South Asia',]
}

covs <- read.csv('/share/homes/annieb6/covariates/cleaned_covs.csv')
covs <- covs[names(covs) %in% c('location_id', 'year_id', 
                                "cv_ldi_pc",
                                "cv_pollution_outdoor_pm25",
                                "cv_tfr",
                                "cv_stunting_prop_haz_under_2sd",
                                "J01M")]
#centre scale covs
source('/share/code/geospatial/annieb6/misc/centre_scale.R')
design_matrix = data.frame(covs[,3:7])
cs_df <- getCentreScale(design_matrix)
design_matrix <- centreScale(design_matrix, df = cs_df)
covs[,3:7] <- design_matrix
rm(design_matrix, cs_df)

#merge data and covs
mydata <- merge(mydata, covs, by = c('location_id', 'year_id'), all.x = T, all.y = F)

#centre scale year
mydata$year <- mydata$year_id
covs$year <- covs$year_id
design_matrix = data.frame(mydata[,2])
cs_df <- getCentreScale(design_matrix)
design_matrix <- centreScale(design_matrix, df = cs_df)
mydata[,2] <-  design_matrix

design_matrix = data.frame(covs[,2])
cs_df <- getCentreScale(design_matrix)
design_matrix <- centreScale(design_matrix, df = cs_df)
covs[,2] <-  design_matrix

#build the glm ####
#Gaussian model
# model1 <- glmer(val ~ 1 + year_id+
#                           cv_anc4_coverage_prop +
#                           cv_dtp3_coverage_prop +
#                           cv_hospital_beds_per1000 +
#                           cv_ldi_pc +
#                           cv_mean_temperature +
#                           # cv_pollution_outdoor_pm25 +   # thhis has wierd patterns post 2010
#                           cv_sanitation_prop +
#                           cv_stunting_prop_haz_under_2sd +
#                           # cv_antimalarial_effective_tmt_map +
#                           cv_intest_typhoid +
#                           ddd_per_1000 +
#                           J01C +
#                           J01E +
#                           (1 |super_region / region / country), data = mydata, family = 'gaussian'(link='logit'))
# 

#or binomial model:
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model1 <- glmer(response ~ 1 + year_id +
                cv_ldi_pc+
                cv_pollution_outdoor_pm25+
                cv_tfr+
                cv_stunting_prop_haz_under_2sd+
                J01M+
                QA+
                  (1 |super_region / region / country), data = mydata, family = 'binomial')

summary(model1)

#predict out to all typhi endmic locations ####
locs             <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
endemic <- read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/typhi_endemic_locations.csv', stringsAsFactors = F)
locs <- locs[locs$loc_id %in% endemic$loc_id,]
locs <- locs[locs$level ==3,]
if(grep('paratyphi', indicator) == 1){
  locs <- locs[locs$region_id == 159 |
                 locs$region_id == 9 |
                 locs$region_id == 5,]
}
rm(endemic)

covs <- merge(covs, locs, by.x = 'location_id', by.y = 'loc_id')
colnames(covs)[colnames(covs) == 'ihme_lc_id'] <- 'country'
colnames(covs)[colnames(covs) == 'region_id'] <- 'region'
colnames(covs)[colnames(covs) == 'spr_reg_id'] <- 'super_region'
covs$region[covs$region == 5] <- 'East Asia '
covs$region[covs$region == 9] <- 'Southeast Asia'
covs$region[covs$region == 65] <- 'High-income Asia Pacific'
covs$region[covs$region == 138] <- 'North Africa & Middle East'
covs$region[covs$region == 159] <- 'South Asia'
covs$region[covs$region == 167] <- 'Central Sub-Saharan Africa'
covs$region[covs$region == 174] <- 'Eastern Sub-Saharan Africa'
covs$region[covs$region == 192] <- 'Southern Sub-Saharan Africa'
covs$region[covs$region == 199] <- 'Western Sub-Saharan Africa'

covs$super_region[covs$super_region == 64] <- 'High Income'
covs$super_region[covs$super_region == 137] <- 'North Africa & Middle East'
covs$super_region[covs$super_region == 158] <- 'South Asia'
covs$super_region[covs$super_region == 4] <- 'Southeast Asia, East Asia & Oceania'
covs$super_region[covs$super_region == 166] <- 'Sub-Saharan Africa'

# covs <- covs[names(covs) %in% names(mydata)]

covs$country <- as.character(covs$country) 
covs$QA <- 1
covs$pred <- predict(model1, newdata = covs, type = 'response', allow.new.levels = TRUE)
summary(covs$pred)

# mydata <- mydata[mydata$location_id %in%covs$location_id,]
covs <- merge(covs, mydata[c('location_id', 'year', 'val', 'sample_size')], by = c('location_id', 'year'), all.x = T, all.y = T)


covs$sample_size_bins <- NA
covs$sample_size_bins[covs$sample_size<50] <- '10-49'
covs$sample_size_bins[covs$sample_size>=50 &covs$sample_size<100 ] <- '50-99'
covs$sample_size_bins[covs$sample_size>=100 &covs$sample_size<500 ] <- '100-499'
covs$sample_size_bins[covs$sample_size>=500 ] <- '500+'
covs$sample_size_bins <-  as.factor(covs$sample_size_bins)
covs$sample_size_bins <- factor(covs$sample_size_bins, levels = c("10-49", "50-99", "100-499", "500+"))

# Plot 
pdf(paste0('/share/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/', indicator, '/glm_model.pdf'),
    height = 17,
    width = 12)

for(i in 1:(ceiling(length(unique(covs$super_region))))){
  print(ggplot(covs[covs$super_region==unique(covs$super_region)[i],])+
          geom_line(aes(x = year_id, y = pred))+
          geom_point(aes(x = year_id, y = val))+
          facet_wrap_paginate(~country, page = i)
  )}

dev.off()

saveRDS(model1, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/', indicator, '/glm_model.rds'))

# Define outliers ####

# # Option 1: Cooks distance ####
#  identify influential datapoints as those with 3*cooks distance  #### THIS DOESNT WORK WITH GLMMS
# cooksd <- cooks.distance(model1)
# 
# png('/ihme/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/cooks_distance.png',
#     height = 10, width = 20, units = 'cm', res = 150)
# plot(cooksd, pch="*", cex=2, main="Influential Obs by Cooks distance")  # plot cook's distance
# abline(h = 4*mean(cooksd, na.rm=T), col="red")  # add cutoff line
# text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")  # add labels
# dev.off()
# 
# mydata$cooksd <- cooksd
# mydata$outlier <- '0'
# mydata$outlier[mydata$cooksd > 3*mean(cooksd, na.rm=T)] <- '1'
# # covs$outlier <-  NULL
# covs <- merge(covs, mydata[c('location_id', 'year', 'outlier')], by = c('location_id', 'year'), all.x = T, all.y = T)
# covs <- covs[!is.na(covs$super_region),]
# 
# pdf('/share/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/outliers_CD3.pdf',
#     height = 17,
#     width = 12)
# 
# for(i in 1:(ceiling(length(unique(covs$super_region))))){
#   print(ggplot(covs[covs$super_region==unique(covs$super_region)[i],])+
#           geom_line(aes(x = year, y = pred))+
#           geom_point(aes(x = year, y = val, colour = outlier))+
#           facet_wrap_paginate(~country, page = i)
#   )}
# 
# dev.off() 
# 
# mydata$cooksd <-  NULL
# mydata$outlier <- NULL
# covs$outlier <- NULL

#
# Option 2: Points are considered outlier if they are GLMM pred +/- n*MSE ####
# Calculate the MSE for each country and apply this as this differs greatly by country
# Try various values of n
# library(MLmetrics)
# library(data.table)
# 
# covs <- data.table(covs) 
# 
# MSEs <-  covs[,.(upper_bound = pred + 4*MSE(val[!is.na(val)], pred[!is.na(val)]),
#                    lower_bound = pred - 4*MSE(val[!is.na(val)], pred[!is.na(val)])),
#                    by = c('country')]
# 
# MSEs <- MSEs[,2:3]
# covs <- cbind(covs, MSEs)
# 
# pdf('/share/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/glm_model_bounded_4_MSE_by_country.pdf',
#     height = 17,
#     width = 12)
# 
# for(i in 1:(ceiling(length(unique(covs$super_region))))){ 
#   print(ggplot(covs[covs$super_region==unique(covs$super_region)[i],])+ 
#           geom_line(aes(x = year, y = pred))+
#           geom_ribbon(aes(x = year, ymin = lower_bound, ymax = upper_bound, colour = 'red', fill = 'red', alpha  =0.5))+
#           geom_point(aes(x = year, y = val))+
#           facet_wrap_paginate(~country, page = i)
#   )} 
# 
# dev.off()
# 
# covs$upper_bound <- NULL
# covs$lower_bound <- NULL


# Option 3: Points are considered outlier if they are GLMM pred +/- n*MAD ####
# Calculate the MAD for each country and apply this as this differs greatly by country
# MAD is the median absolute deviation and in this case is preferable to MSE due to highly heterogenous data
# Try various values of n
# Want to outlier ~10% of the data
library(stats)
covs <- data.table(covs)
covs$upper_bound <-  NULL
covs$lower_bound <-  NULL
mydata$upper_bound <-  NULL
mydata$lower_bound <-  NULL
mydata$is_outlier <- 0

MADs <-  covs[,.(upper_bound = pred + 1*mad(pred[!is.na(val)], val[!is.na(val)]),
                 lower_bound = pred - 1*mad(pred[!is.na(val)],val[!is.na(val)])),
              by = c('country')]

MADs <- MADs[,2:3]
covs <- cbind(covs, MADs)
covs$upper_bound[covs$upper_bound>1] <- 1
covs$lower_bound[covs$lower_bound<0] <- 0

covs <- covs[!is.na(covs$super_region),]
pdf(paste0('/share/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/', indicator, '/glm_model_bounded_1_MAD.pdf'),
    height = 17,
    width = 12) 

for(i in 1:(ceiling(length(unique(covs$super_region))))){ 
  print(ggplot(covs[covs$super_region==unique(covs$super_region)[i],])+ 
          geom_line(aes(x = year, y = pred))+
          geom_ribbon(aes(x = year, ymin = lower_bound, ymax = upper_bound, colour = 'red', fill = 'red', alpha  =0.5))+
          geom_point(aes(x = year, y = val))+
          facet_wrap_paginate(~country, page = i)
  )} 

dev.off()

#define outliers in the dataset
MADs <- covs[,.(country, year, lower_bound, upper_bound)]
MADs <-  unique(MADs)

mydata <- merge(mydata, MADs, by = c('country', 'year'))
mydata$is_outlier[mydata$val<mydata$lower_bound |mydata$val>mydata$upper_bound] <- 1
mydata$is_outlier[mydata$val>mydata$lower_bound & mydata$val<mydata$upper_bound] <- 0
table(mydata$is_outlier)
outliers <- mydata[mydata$is_outlier == 1,]
outliers <- unique(outliers[c('nid', 'year')])
write.csv(outliers, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/', indicator, '/outlier_nids.csv'), row.names = F)

mydata$year_id <-  NULL
colnames(mydata)[colnames(mydata) == 'year'] <-  'year_id'

mydata <- mydata[c("location_id",
                   "year_id",
                   "nid",
                   "super_region",                     
                   "region",     
                   "country",
                   "age_group_id",       
                   "sex_id",                           
                   "measure",         
                   "is_outlier",     
                   "sample_size",                      
                   "val",                
                   "variance",
                   "QA")]

write.csv(mydata, paste0('/share/homes/annieb6/AMR/typhi_paratyphi/datasets/', indicator, '_outliered.csv'), row.names = F)

