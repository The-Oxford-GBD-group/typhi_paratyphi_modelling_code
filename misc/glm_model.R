rm(list = ls())
library(lme4)
library(boot)
library(foreign)
library(ggplot2)
library(ggforce)

#set output directory
model_date = format(Sys.Date(), "%Y_%m_%d")
outputdir <-  paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', model_date, '/')
dir.create(outputdir, showWarnings = F, recursive = T)

#Load data
mydata <- data.table(read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/MDR_typhi_outliered.csv', stringsAsFactors = F))
mydata <- mydata[mydata$is_outlier == 0,]

#load covariates - either centre-scaled or standard
covs <- read.csv('/ihme/homes/annieb6/covariates/cleaned_covs.csv', stringsAsFactors = F)


covs_to_include <-  c("cv_anc4_coverage_prop",
                      "cv_dtp3_coverage_prop",
                      "cv_hospital_beds_per1000",
                      "cv_ldi_pc",
                      "cv_mean_temperature",
                      # "cv_pollution_outdoor_pm25",
                      "cv_sanitation_prop",
                      "cv_stunting_prop_haz_under_2sd",
                      "cv_antimalarial_effective_tmt_map",
                      'cv_intest_typhoid',
                      "ddd_per_1000",
                      "J01C",
                      "J01E")



#restrict covs to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']
covs <- data.table(covs)
covs <- na.omit(covs)

# transform covs
#centre scale the covariates if desired
  covs <- data.frame(covs)
  covs[colnames(covs) %in% covs_to_include] <- data.frame(scale(covs[colnames(covs) %in% covs_to_include]))
  covs$year <- scale(covs$year_id)
  covs <-  data.table(covs)
  
  mydata$QA <-  scale(mydata$QA)

#merge data and covs
mydata <- merge(mydata, covs, by = c('location_id', 'year_id'), all.x = T, all.y = F)
  
covs$QA <- min(mydata$QA)

#get the location codes
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
locs <-  locs[c('loc_id', "spr_reg_id", "region_id", "ihme_lc_id", 'level')]

mydata <- merge(mydata, locs, by.x = 'location_id', by.y = c('loc_id'), all.x = T, all.y = F)
covs <- merge(covs, locs, by.x = 'location_id', by.y = c('loc_id'), all.x = T, all.y = F)

model1 <- glmer(val ~ 1 + year+
                  cv_anc4_coverage_prop +
                  cv_dtp3_coverage_prop +
                  cv_hospital_beds_per1000 +
                  cv_ldi_pc +
                  cv_mean_temperature +
                  cv_sanitation_prop +
                  cv_stunting_prop_haz_under_2sd +
                  cv_intest_typhoid +
                  ddd_per_1000 +
                  J01C +
                  J01E +
                  QA +
                  (1 | spr_reg_id/region_id/location_id), data = mydata, family = 'gaussian'(link='logit'))

#predict model out
mydata$IS <- inv.logit(predict(model1, mydata))
plot(mydata$val, mydata$IS)
cor(mydata$val, mydata$IS)^2

covs$pred <- inv.logit(predict(model1, covs, allow.new.levels = TRUE))

#check what OOS looks like
mydata <-  data.table(mydata)
mydata <- mydata[sample(nrow(mydata)),]
mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]

## add a row id column
mydata[, a_rowid := seq(1:nrow(mydata))]
mydata$OOS <- NA

for(i in 1:5){
  subset_data <- mydata[mydata$fold_id != i,]
  
  model1 <- glmer(val ~ 1 + year+
                    cv_anc4_coverage_prop +
                    cv_dtp3_coverage_prop +
                    cv_hospital_beds_per1000 +
                    cv_ldi_pc +
                    cv_mean_temperature +
                    cv_sanitation_prop +
                    cv_stunting_prop_haz_under_2sd +
                    cv_intest_typhoid +
                    ddd_per_1000 +
                    J01C +
                    J01E +
                    QA +
                    (1 | spr_reg_id/region_id/location_id), data = subset_data, family = 'gaussian'(link='logit'))
  
  #predict out for that fold
  mydata$OOS[mydata$fold_id == i] <- inv.logit(predict(model1, mydata[mydata$fold_id == i], allow.new.levels = TRUE))
}


plot(mydata$val, mydata$OOS)
cor(mydata$val, mydata$OOS)^2

plot_data <- merge(covs, mydata[,.(location_id, year_id, val)], by = c('location_id', 'year_id'), all.x = T, all.y = T)
plot_data <-  plot_data[plot_data$level == 3,]

pdf(paste0(outputdir, '/GLMER.pdf'),
    height = 8.3, width = 11.7)

for(i in 1:(ceiling(length(unique(plot_data$region_id))))){
  print(ggplot(plot_data[plot_data$region_id==unique(covs$region_id)[i],])+
          geom_line(aes(x = year, y = pred))+
          geom_point(aes(x = year, y = val))+
          facet_wrap_paginate(~ihme_lc_id, page = i)
  )}

dev.off() 


#save file
stg1 <-  covs[, .(location_id, year_id,
                  age_group_id = rep(22, length(covs$location_id)),
                  sex_id = rep(3, length(covs$location_id)),
                  cv_custom_stage_1 = pred)]


#check that the estimates are within expected range
max(stg1$cv_custom_stage_1, na.rm = T)
min(stg1$cv_custom_stage_1, na.rm = T)

stg1$cv_custom_stage_1[stg1$cv_custom_stage_1<=0] <- 0.001

#save prediction
write.csv(stg1, paste0(outputdir, '/custom_stage1_df.csv'), row.names = F)

