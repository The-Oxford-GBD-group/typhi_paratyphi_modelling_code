# Get the input data with the fold ID's in it
rm(list = ls())
library(ggplot2)
library(caret)
library(ini, lib = '/share/homes/annieb6/temp_packages/')
library(boot)
library(foreign)
source('/share/code/st_gpr/central/stgpr/r_functions/utilities/utility.r')
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
run_date = 'MDR_Typhi_final_10HO'
mydata <- read.csv(paste0('/share/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', run_date, '/master_data.csv'), stringsAsFactors = F)
mydata <- mydata[mydata$is_outlier == 0,]
colnames(mydata)[colnames(mydata)=='p'] <- 'val'
run_ids <- c('174632',
             '174644',
             '174647',
             '174650',
             '174653',
             '174656',
             '174659',
             '174662',
             '174665',
             '174668'
)

#load in the GPR results and look at the OOS fits
for(i in 1:length(run_ids)){
  gpr <- model_load(run_ids[i], 'gpr')
  # gpr <- gpr[,1:5]
  gpr$age_group_id <- NULL
  gpr$sex_id <- NULL
  mydata <- merge(mydata, gpr, by = c('location_id', 'year_id'))
  colnames(mydata)[colnames(mydata) == 'gpr_mean'] <- paste0('se_stgpr_ho', i)
  colnames(mydata)[colnames(mydata) == 'gpr_lower'] <- paste0('lower_ho', i)
  colnames(mydata)[colnames(mydata) == 'gpr_upper'] <- paste0('upper_ho', i)
}

mydata$se_stgpr_oos <- NA
mydata$se_stgpr_oos[mydata$master_fold_id == 1] <- mydata$se_stgpr_ho1[mydata$master_fold_id == 1]
mydata$se_stgpr_oos[mydata$master_fold_id == 2] <- mydata$se_stgpr_ho2[mydata$master_fold_id == 2]
mydata$se_stgpr_oos[mydata$master_fold_id == 3] <- mydata$se_stgpr_ho3[mydata$master_fold_id == 3]
mydata$se_stgpr_oos[mydata$master_fold_id == 4] <- mydata$se_stgpr_ho4[mydata$master_fold_id == 4]
mydata$se_stgpr_oos[mydata$master_fold_id == 5] <- mydata$se_stgpr_ho5[mydata$master_fold_id == 5]
mydata$se_stgpr_oos[mydata$master_fold_id == 6] <- mydata$se_stgpr_ho6[mydata$master_fold_id == 6]
mydata$se_stgpr_oos[mydata$master_fold_id == 7] <- mydata$se_stgpr_ho7[mydata$master_fold_id == 7]
mydata$se_stgpr_oos[mydata$master_fold_id == 8] <- mydata$se_stgpr_ho8[mydata$master_fold_id == 8]
mydata$se_stgpr_oos[mydata$master_fold_id == 9] <- mydata$se_stgpr_ho9[mydata$master_fold_id == 9]
mydata$se_stgpr_oos[mydata$master_fold_id ==10] <- mydata$se_stgpr_ho10[mydata$master_fold_id ==10]

mydata$upper_oos <- NA
mydata$upper_oos[mydata$master_fold_id == 1] <- mydata$upper_ho1[mydata$master_fold_id == 1]
mydata$upper_oos[mydata$master_fold_id == 2] <- mydata$upper_ho2[mydata$master_fold_id == 2]
mydata$upper_oos[mydata$master_fold_id == 3] <- mydata$upper_ho3[mydata$master_fold_id == 3]
mydata$upper_oos[mydata$master_fold_id == 4] <- mydata$upper_ho4[mydata$master_fold_id == 4]
mydata$upper_oos[mydata$master_fold_id == 5] <- mydata$upper_ho5[mydata$master_fold_id == 5]
mydata$upper_oos[mydata$master_fold_id == 6] <- mydata$upper_ho6[mydata$master_fold_id == 6]
mydata$upper_oos[mydata$master_fold_id == 7] <- mydata$upper_ho7[mydata$master_fold_id == 7]
mydata$upper_oos[mydata$master_fold_id == 8] <- mydata$upper_ho8[mydata$master_fold_id == 8]
mydata$upper_oos[mydata$master_fold_id == 9] <- mydata$upper_ho9[mydata$master_fold_id == 9]
mydata$upper_oos[mydata$master_fold_id ==10] <- mydata$upper_ho10[mydata$master_fold_id ==10]

mydata$lower_oos <- NA
mydata$lower_oos[mydata$master_fold_id == 1] <- mydata$lower_ho1[mydata$master_fold_id == 1]
mydata$lower_oos[mydata$master_fold_id == 2] <- mydata$lower_ho2[mydata$master_fold_id == 2]
mydata$lower_oos[mydata$master_fold_id == 3] <- mydata$lower_ho3[mydata$master_fold_id == 3]
mydata$lower_oos[mydata$master_fold_id == 4] <- mydata$lower_ho4[mydata$master_fold_id == 4]
mydata$lower_oos[mydata$master_fold_id == 5] <- mydata$lower_ho5[mydata$master_fold_id == 5]
mydata$lower_oos[mydata$master_fold_id == 6] <- mydata$lower_ho6[mydata$master_fold_id == 6]
mydata$lower_oos[mydata$master_fold_id == 7] <- mydata$lower_ho7[mydata$master_fold_id == 7]
mydata$lower_oos[mydata$master_fold_id == 8] <- mydata$lower_ho8[mydata$master_fold_id == 8]
mydata$lower_oos[mydata$master_fold_id == 9] <- mydata$lower_ho9[mydata$master_fold_id == 9]
mydata$lower_oos[mydata$master_fold_id ==10] <- mydata$lower_ho10[mydata$master_fold_id ==10]

se_stgpr_r2 <- cor(mydata$val, mydata$se_stgpr_oos)^2
se_stgpr_rmse <- RMSE(mydata$val, mydata$se_stgpr_oos)
mydata$coverage <- mydata$val>mydata$lower_oos & mydata$se_stgpr_oos<mydata$upper_oos
length(mydata$coverage[mydata$coverage==TRUE])/length(mydata$coverage)


min <- min(c(mydata$val, mydata$se_stgpr_oos))
max <- max(c(mydata$val, mydata$se_stgpr_oos))

#plot out the predicted vs estimated
png(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', run_date, '/oos_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(mydata)+
    geom_point(aes(x = val, y=se_stgpr_oos))+
    xlim(0, 1)+
    ylim(0, 1)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight",
      title = ("OOS Validation Plot for Stacked Ensemble-STGPR"),
      subtitle = paste0("RMSE = ", round(se_stgpr_rmse,2), ";    R2 = ", round(se_stgpr_r2,2)))
) 
dev.off()


#merge the regions onto the data and plot by region
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
locs <- locs[c('loc_id', "spr_reg_id", "region_id")]

mydata <- merge(mydata, locs, by.x = 'location_id', by.y = 'loc_id')

mydata$region_id[mydata$region_id == 5] <- 'East Asia '
mydata$region_id[mydata$region_id == 9] <- 'Southeast Asia'
mydata$region_id[mydata$region_id == 65] <- 'High-income Asia Pacific'
mydata$region_id[mydata$region_id == 138] <- 'North Africa & Middle East'
mydata$region_id[mydata$region_id == 159] <- 'South Asia'
mydata$region_id[mydata$region_id == 167] <- 'Central Sub-Saharan Africa'
mydata$region_id[mydata$region_id == 174] <- 'Eastern Sub-Saharan Africa'
mydata$region_id[mydata$region_id == 192] <- 'Southern Sub-Saharan Africa'
mydata$region_id_id[mydata$region_id == 199] <- 'Western Sub-Saharan Africa'

mydata$spr_reg_id[mydata$spr_reg_id == 64] <- 'High Income'
mydata$spr_reg_id[mydata$spr_reg_id == 137] <- 'North Africa & Middle East'
mydata$spr_reg_id[mydata$spr_reg_id == 158] <- 'South Asia'
mydata$spr_reg_id[mydata$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
mydata$spr_reg_id[mydata$spr_reg_id == 166] <- 'Sub-Saharan Africa'


#a. by super region
mydata <- data.table(mydata)
metrics <- mydata[,.(RMSE = round(RMSE(val, se_stgpr_oos),2),
                     Rsq = round(cor(val, se_stgpr_oos)^2,2)),
                  by = 'spr_reg_id']

write.csv(metrics, paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', run_date, '/oos_metrics_by_spr_reg.csv'), row.names = F)

#plot out the predicted vs estimated
png(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/holdouts/', run_date, '/oos_correlation_by_spr_reg.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
  ggplot(mydata)+
    geom_point(aes(x = val, y=se_stgpr_oos))+
    xlim(0, 1)+
    ylim(0, 1)+
    geom_abline(slope = 1, intercept = 0, colour = 'red')+
    theme_bw() +
    theme(strip.background = element_rect(fill = "white")) +
    labs(
      x = "Data Estimate",
      y = "Mean Prediction",
      size = "Weight")+
    facet_wrap(~spr_reg_id)
) 
dev.off()

