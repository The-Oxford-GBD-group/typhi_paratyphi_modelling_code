rm(list = ls())
#Load required packages
library("DDoutlier")
library("scales")
library("ggrepel")
library(data.table)

#Read in the mydata
mydata<-read.csv('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/stgpr_files/subnat_FQNS_paratyphi.csv')
covs <- read.csv('Z:/AMR/Pathogens/typhi_paratyphi/covariates/cleaned_covs.csv')

mydata <- merge(mydata, covs, by.x = c('national_id','year_id', 'age_group_id', 'sex_id'), age, by.y = c('location_id','year_id', 'age_group_id', 'sex_id'))
# mydata <- merge(mydata, covs)

mydata$ln_ddd_per_1000_raw <- log(mydata$ddd_per_1000)

#rescale covs
mydata[,15:74] <- apply(mydata[,15:74],2,function(x) rescale(x))
mydata$year.rescaled<- rescale(mydata$year_id)
mydata$super_region_rescaled <- rescale(as.numeric(as.factor(mydata$super_region)))
mydata$region_rescaled <- rescale(as.numeric(as.factor(mydata$region)))
mydata$location_id_rescaled <-  rescale(mydata$national_id)
df <- mydata[c('val',
               'year.rescaled',
               'ln_ddd_per_1000_raw',
               "cv_pollution_outdoor_pm25",
               "cv_diabetes",
               "cv_water_prop",
               "cv_hospital_beds_per1000",
               "cv_mean_temperature",              
               'cv_abx_prop',
               'cv_latitude',
               'location_id_rescaled')]

df <- df[complete.cases(df),]
rownames(df) <- c()

##Natural Neighbor (NAN) algorithm to return the self-adaptive neighborhood
K <- NAN(df, NaN_Edges=FALSE)$r

##Influenced Outlierness (LDF) algorithm: greater LDF = greater outlierness
outlier_score_LDF <- LDF(dataset=df, k=K, h=2, c=1)$LDF

# rescale outlier scores
outlier_score_LDF <- LDF(dataset=df, k=K, h=2, c=1)$LDF

# define outliers in mydata
mydata <- mydata[,1:15]
mydata$is_outlier[outlier_score_LDF>=quantile(outlier_score_LDF,.95)] <- 1
mydata$is_outlier[outlier_score_LDF<quantile(outlier_score_LDF,.95)] <- 0

#if there are <=2 datapoints per country keep them in
mydata <- data.table(mydata)
pp_country <- mydata[,.(points = .N),
                     by = c('country')] 
pp_country <- pp_country[points <=2,]
mydata$is_outlier[mydata$country %in% pp_country$country] <- 0

#save the data
write.csv(mydata, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/stgpr_files/subnat_FQNS_paratyphi_outliered.csv', row.names = F)
