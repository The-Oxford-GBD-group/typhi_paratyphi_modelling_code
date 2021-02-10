# Creat files for relative uncertainty maps for MDR and FQNS Typhi and Paratyphi #

rm(list = ls())
library(raster)
setwd('Z:/AMR/Pathogens/typhi_paratyphi/model_results')
dir.create('uncertainty_maps')

my_shp <- shapefile('C:/users/annieb/desktop/GBD2020_analysis_final.shp')

for(db in c('MDR_Typhi', 'FQNS_Typhi', 'MDR_Paratyphi', 'FQNS_Paratyphi')){
  mydata <- read.csv(paste0('STGPR/', db, '/model_estimates.csv'))
  
  #restrict to national 2018
  mydata <-  mydata[mydata$year_id == 2018,]
  mydata <-  mydata[mydata$level == 3,]
  
  #rank the countries 1:4 based on resistance use (low to high)
  mydata$resistance <- NA
  mydata$resistance[mydata$gpr_mean <= quantile(mydata$gpr_mean, 0.25)] <- 1
  mydata$resistance[mydata$gpr_mean > quantile(mydata$gpr_mean, 0.25) & mydata$gpr_mean <= quantile(mydata$gpr_mean, 0.5)] <- 2
  mydata$resistance[mydata$gpr_mean > quantile(mydata$gpr_mean, 0.5) & mydata$gpr_mean <= quantile(mydata$gpr_mean, 0.75)] <- 3
  mydata$resistance[mydata$gpr_mean > quantile(mydata$gpr_mean, 0.75)] <- 4
  
  #calculate uncertainty
  mydata$relative_uncertainty <- (mydata$gpr_upper - mydata$gpr_lower)/mydata$gpr_mean
  
  #rank the districts 1:4 based on relative uncertainty (low to high)
  mydata$uncertainty <- NA
  mydata$uncertainty[mydata$relative_uncertainty <= quantile(mydata$relative_uncertainty, 0.25)] <- 1
  mydata$uncertainty[mydata$relative_uncertainty > quantile(mydata$relative_uncertainty, 0.25) & mydata$relative_uncertainty <= quantile(mydata$relative_uncertainty, 0.5)] <- 2
  mydata$uncertainty[mydata$relative_uncertainty > quantile(mydata$relative_uncertainty, 0.5) & mydata$relative_uncertainty <= quantile(mydata$relative_uncertainty, 0.75)] <- 3
  mydata$uncertainty[mydata$relative_uncertainty > quantile(mydata$relative_uncertainty, 0.75)] <- 4
  
  mydata <- mydata[c('location_id', 'resistance', 'uncertainty')]
  #split out the layers for the uncertainty map
  # resistance1 <- mydata[mydata$resistance == 1,]
  # resistance2 <- mydata[mydata$resistance == 2,]
  # resistance3 <- mydata[mydata$resistance == 3,]
  # resistance4 <- mydata[mydata$resistance == 4,]
 
  resistance1 <- merge(my_shp, mydata[mydata$resistance == 1,], by.x = 'loc_id', by.y = 'location_id', all.x = F)
  resistance2 <- merge(my_shp, mydata[mydata$resistance == 2,], by.x = 'loc_id', by.y = 'location_id', all.x = F)
  resistance3 <- merge(my_shp, mydata[mydata$resistance == 3,], by.x = 'loc_id', by.y = 'location_id', all.x = F)
  resistance4 <- merge(my_shp, mydata[mydata$resistance == 4,], by.x = 'loc_id', by.y = 'location_id', all.x = F)
  
  dir.create(paste0('uncertainty_maps/', db), showWarnings = FALSE)
  shapefile(resistance1, paste0('uncertainty_maps/', db, '/resisance1.shp'), overwrite = T)
  shapefile(resistance2, paste0('uncertainty_maps/', db, '/resisance2.shp'), overwrite = T)
  shapefile(resistance3, paste0('uncertainty_maps/', db, '/resisance3.shp'), overwrite = T)
  shapefile(resistance4, paste0('uncertainty_maps/', db, '/resisance4.shp'), overwrite = T)
}
