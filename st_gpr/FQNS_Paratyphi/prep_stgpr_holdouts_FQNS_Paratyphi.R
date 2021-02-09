#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the MDR typhi files for STGPR with holdouts #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)
rm(list = ls())

SA_file = 'Final_FQNS_Paratyphi_SA'
SEA_file = 'Final_FQNS_Paratyphi_SEA'

dir.create(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d")))
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')

# Merge the two input data files
SA_master <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', SA_file,'/master_data.csv'), stringsAsFactors = F)
SEA_master <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', SEA_file,'/master_data.csv'), stringsAsFactors = F)

SA_master <- SA_master[c('location_id', 'year_id', 'nid', 'country', 'age_group_id', 'sex_id', 'measure_id', 'is_outlier', 'p', 'd', 'variance', 'master_fold_id')]
SEA_master <- SEA_master[c('location_id', 'year_id', 'nid', 'country', 'age_group_id', 'sex_id', 'measure_id', 'is_outlier', 'p', 'd',  'variance','master_fold_id')]
master_data <- rbind(SEA_master, SA_master)
colnames(master_data)[colnames(master_data) == 'p'] <- 'val'
colnames(master_data)[colnames(master_data) == 'd'] <- 'sample_size'
rm(SEA_master, SA_master)

write.csv(master_data, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d"), '/', 'master_data.csv'), row.names = F)

for(h in 1:10){
  message('Working on holdout ', h)
  SEA <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', SEA_file,'/holdout_', h, '/custom_stage1_df.csv'))
  SA <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', SA_file,'/holdout_', h, '/custom_stage1_df.csv'))

#restrict to locations from each file
  SEA <- SEA[!(SEA$location_id %in% SA$location_id),]
  
#combine Asia and Africa
mydata <- rbind(SA, SEA)

#check that this is the right length
if(length(mydata$location_id) != 30798){
  stop('Somethings gone wrong, this is not the right size')
}

summary(mydata$year_id)
#Save the holdout stage 1 estimates
write.csv(mydata, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d"), '/', 'custom_stage1_h', h, '.csv'), row.names = F)

#save out input data
input <- master_data[master_data$master_fold_id != h,]
write.csv(input, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d"), '/', 'input_data_h', h, '.csv'), row.names = F)
}

#~~~~~#
# END #
#~~~~~#