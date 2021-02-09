#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the MDR typhi files for STGPR #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)
rm(list = ls())

sSA_file = '2020_07_10_sSA_CWM'
Asia_file = '2020_07_10_Asia_CWM'

sSA_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', sSA_file, '/custom_stage1_df.csv'))
Asia_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', Asia_file, '/custom_stage1_df.csv'))

locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
sSA_file <- sSA_file[sSA_file$location_id %in% locs$loc_id[locs$spr_reg_id == 166 |locs$loc_id == 522],]
Asia_file <- Asia_file[Asia_file$location_id %in% locs$loc_id[locs$spr_reg_id != 166 & locs$loc_id != 522],]

mydata <- rbind(sSA_file, Asia_file)
summary(mydata$cv_custom_stage_1)

write.csv(mydata, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/custom_stage1_', format(Sys.Date(), "%Y_%m_%d"), '_CWM.csv'), row.names = F)

#~~~~~#
# END #
#~~~~~#