#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the FQNS Paratyphi files for STGPR #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
library(foreign)
rm(list = ls())

SA_file = 'Final_min20_FQNS_Paratyphi_SA'
SEA_file = 'Final_min20_FQNS_Paratyphi_SEA'

SA_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', SA_file, '/custom_stage1_df.csv'))
SEA_file <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', SEA_file, '/custom_stage1_df.csv'))

SEA_file <- SEA_file[!(SEA_file$location_id %in% SA_file$location_id),]

mydata <- rbind(SA_file, SEA_file)
summary(mydata$cv_custom_stage_1)

write.csv(mydata, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/custom_stage1_', format(Sys.Date(), "%Y_%m_%d"), '_FQNS_Paratyphi.csv'), row.names = F)

#~~~~~#
# END #
#~~~~~#