library(data.table)
rm(list = ls())

# Clean up the data and outlier for admin 1 models
mydata <- readRDS("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/MDR_typhi.rds")
mydata <- data.table(mydata)
mydata <- mydata[order(source_id, iso3, mid_year, sample_size, percentage_resistant)]
mydata$p <- round(mydata$percentage_resistant/100,3)
mydata$adm1 <- as.numeric(mydata$adm1)
mydata <- mydata[mydata$best_geo != 'national',]
mydata <- mydata[,.(source_id, super_region, region, iso3, adm1, year = mid_year, 
                    n = number_resistant, d = sample_size, p = round(percentage_resistant/100,3), QA)]
#~~~~~~~~~~~~~#
# Outliers ####
#~~~~~~~~~~~~~#

#check for outliers - going to sticl with MAD method, start with 2.5xMAD outliers (i.e more data)
# trying DDoutlier method
outliers <- fread('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/outliering/DD_outlier/MDR_typhi_outliered_rdeos_info.csv')
outliers <- outliers[,.(nid, country, year_id, sample_size, is_outlier)]
outliers <- unique(data.frame(outliers))

#remove countries not endemic
mydata <- mydata[mydata$iso3 %in% outliers$country,]
mydata <- mydata[mydata$source_id != 3286,]

#merge on outliers
mydata <- merge(mydata, outliers, by.x = c('source_id', 'iso3', 'year', 'd'),
                by.y = c('nid', 'country', 'year_id', 'sample_size'))

mydata <- mydata[mydata$is_outlier == 0,]
mydata$is_outlier <- NULL
rm(outliers)

write.csv(mydata, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/MDR_Typhi_outliered_v2.csv', row.names = F)
