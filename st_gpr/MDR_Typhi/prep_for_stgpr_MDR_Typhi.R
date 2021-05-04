#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the MDR Typhi files for STGPR #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)
rm(list = ls())

sSA_file = 'MDR_Typhi_300421_sSA'
Asia_file = 'MDR_Typhi_300421_Asia'

sSA_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', sSA_file, '/custom_stage1_df.csv'))
Asia_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', Asia_file, '/custom_stage1_df.csv'))

locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
sSA_file <- sSA_file[sSA_file$location_id%in%locs$loc_id[locs$spr_reg_id==166],]

Asia_file <- Asia_file[!(Asia_file$location_id %in% sSA_file$location_id),]

mydata <- rbind(sSA_file, Asia_file)
summary(mydata$cv_custom_stage_1)

write.csv(mydata, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/custom_stage1_MDR_Typhi_300421.csv'), row.names = F)

#~~~~~#
# END #
#~~~~~#