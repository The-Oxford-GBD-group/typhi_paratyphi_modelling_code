#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the MDR typhi files for STGPR with holdouts #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)
rm(list = ls())

sSA_file_name = 'MDR_Typhi_final_random10HO_sSA'
Asia_file_name = 'MDR_Typhi_final_random10HO_Asia'

dir.create(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d")))
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')

# Merge the two input data files
sSA_master <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', sSA_file_name,'/master_data.csv'), stringsAsFactors = F)
Asia_master <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', Asia_file_name,'/master_data.csv'), stringsAsFactors = F)

sSA_master <- sSA_master[c('location_id', 'year_id', 'nid', 'country', 'age_group_id', 'sex_id', 'measure_id', 'is_outlier', 'p', 'd', 'variance', 'master_fold_id')]
Asia_master <- Asia_master[c('location_id', 'year_id', 'nid', 'country', 'age_group_id', 'sex_id', 'measure_id', 'is_outlier', 'p', 'd',  'variance','master_fold_id')]
master_data <- rbind(sSA_master, Asia_master)
colnames(master_data)[colnames(master_data) == 'p'] <- 'val'
colnames(master_data)[colnames(master_data) == 'd'] <- 'sample_size'
rm(sSA_master, Asia_master)

write.csv(master_data, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', format(Sys.Date(), "%Y_%m_%d"), '/', 'master_data.csv'), row.names = F)

for(h in 1:10){
  message('Working on holdout ', h)
sSA_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', sSA_file_name,'/holdout_', h, '/custom_stage1_df.csv'))
Asia_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', Asia_file_name,'/holdout_', h, '/custom_stage1_df.csv'))

#restrict to locations from each file
sSA_file <- sSA_file[sSA_file$location_id %in% locs$loc_id[locs$spr_reg_id == 166 |locs$loc_id == 522],]
Asia_file <- Asia_file[Asia_file$location_id %in% locs$loc_id[locs$spr_reg_id != 166 & locs$loc_id != 522],]

#combine Asia and Africa
mydata <- rbind(sSA_file, Asia_file)

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