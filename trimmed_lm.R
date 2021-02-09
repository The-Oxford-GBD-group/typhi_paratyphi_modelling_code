#~~~~~~~~~~~~~~~~~~~~~~~#
# lm on trimmed data ####
#~~~~~~~~~~~~~~~~~~~~~~~#

rm(list = ls())

#LM on trimmed data from MR-BRT:
input_data_stub   <- '/share/homes/annieb6/AMR/typhi_paratyphi/datasets/mdr_typhi_national'
all_data   <- read.csv('/share/homes/annieb6/AMR/typhi_paratyphi/datasets/mdr_typhi_national_only.csv', stringsAsFactors = F)
covs <- read.csv('/share/homes/annieb6/covariates/centre_scaled_covs.csv', stringsAsFactors = F)
covs_to_include <- c('cv_anc4_coverage_prop',
                 'cv_dtp3_coverage_prop',
                 'cv_hospital_beds_per1000',
                 'cv_mean_temperature',
                 'cv_pollution_outdoor_pm25',
                 'cv_sanitation_prop',
                 'cv_tfr',
                 'cv_water_prop',
                 'cv_antimalarial_effective_tmt_map',
                 'cv_handwashing',
                 'cv_diabetes',
                 'cv_intest_typhoid')

covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']
covs <- data.table(covs)
all_data <- merge(all_data, covs)

mydata <- read.csv('/ihme/homes/annieb6/MRBeRT/2019_10_12/mdr_typhi1/train_data.csv', stringsAsFactors = T)
mydata <-  mydata[mydata$w == 1,]
model1 <- lm(logit_mean~cv_anc4_coverage_prop+
                       cv_dtp3_coverage_prop+
                       cv_hospital_beds_per1000+
                       cv_mean_temperature+
                       cv_pollution_outdoor_pm25+
                       cv_sanitation_prop+
                       cv_tfr+
                       cv_water_prop+
                       cv_antimalarial_effective_tmt_map+
                       cv_handwashing+
                       cv_diabetes+
                       cv_intest_typhoid, data = mydata)


mydata <- read.csv('/ihme/homes/annieb6/MRBeRT/2019_10_12/mdr_typhi2/train_data.csv', stringsAsFactors = T)
mydata <-  mydata[mydata$w == 1,]
model2 <- lm(logit_mean~cv_anc4_coverage_prop+
               cv_dtp3_coverage_prop+
               cv_hospital_beds_per1000+
               cv_mean_temperature+
               cv_pollution_outdoor_pm25+
               cv_sanitation_prop+
               cv_tfr+
               cv_water_prop+
               cv_antimalarial_effective_tmt_map+
               cv_handwashing+
               cv_diabetes+
               cv_intest_typhoid, data = mydata)


mydata <- read.csv('/ihme/homes/annieb6/MRBeRT/2019_10_12/mdr_typhi3/train_data.csv', stringsAsFactors = T)
mydata <-  mydata[mydata$w == 1,]
model3 <- lm(logit_mean~cv_anc4_coverage_prop+
               cv_dtp3_coverage_prop+
               cv_hospital_beds_per1000+
               cv_mean_temperature+
               cv_pollution_outdoor_pm25+
               cv_sanitation_prop+
               cv_tfr+
               cv_water_prop+
               cv_antimalarial_effective_tmt_map+
               cv_handwashing+
               cv_diabetes+
               cv_intest_typhoid, data = mydata)

mydata <- read.csv('/ihme/homes/annieb6/MRBeRT/2019_10_12/mdr_typhi4/train_data.csv', stringsAsFactors = T)
mydata <-  mydata[mydata$w == 1,]
model4 <- lm(logit_mean~cv_anc4_coverage_prop+
               cv_dtp3_coverage_prop+
               cv_hospital_beds_per1000+
               cv_mean_temperature+
               cv_pollution_outdoor_pm25+
               cv_sanitation_prop+
               cv_tfr+
               cv_water_prop+
               cv_antimalarial_effective_tmt_map+
               cv_handwashing+
               cv_diabetes+
               cv_intest_typhoid, data = mydata)

mydata <- read.csv('/ihme/homes/annieb6/MRBeRT/2019_10_12/mdr_typhi5/train_data.csv', stringsAsFactors = T)
mydata <-  mydata[mydata$w == 1,]
model5 <- lm(logit_mean~cv_anc4_coverage_prop+
               cv_dtp3_coverage_prop+
               cv_hospital_beds_per1000+
               cv_mean_temperature+
               cv_pollution_outdoor_pm25+
               cv_sanitation_prop+
               cv_tfr+
               cv_water_prop+
               cv_antimalarial_effective_tmt_map+
               cv_handwashing+
               cv_diabetes+
               cv_intest_typhoid, data = mydata)

rm(mydata)


#predict out lm
all_data <- data.table(all_data)
all_data[, 'lm_ho1' := predict(model1, all_data)]
all_data[, 'lm_ho2' := predict(model2, all_data)]
all_data[, 'lm_ho3' := predict(model3, all_data)]
all_data[, 'lm_ho4' := predict(model4, all_data)]
all_data[, 'lm_ho5' := predict(model5, all_data)]

#define the OOS estimates
all_data$trimed_lm_oos <- NA
all_data$trimed_lm_oos[all_data$fold == 1] <- all_data$lm_ho1[all_data$fold == 1]
all_data$trimed_lm_oos[all_data$fold == 2] <- all_data$lm_ho2[all_data$fold == 2]
all_data$trimed_lm_oos[all_data$fold == 3] <- all_data$lm_ho3[all_data$fold == 3]
all_data$trimed_lm_oos[all_data$fold == 4] <- all_data$lm_ho4[all_data$fold == 4]
all_data$trimed_lm_oos[all_data$fold == 5] <- all_data$lm_ho5[all_data$fold == 5]

all_data$trimed_lm_oos <- inv.logit(all_data$trimed_lm_oos)

#plot out OOS metrics
plot(all_data$data, all_data$trimed_lm_oos)
cor(all_data$data, all_data$trimed_lm_oos)^2
rmse(all_data$data, all_data$trimed_lm_oos)

