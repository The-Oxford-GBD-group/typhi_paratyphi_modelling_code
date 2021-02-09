#~~~~~~~~~~~~~~~~~~~~~~~~#
# Analyse ST-GPR outputs #  
# Plot out scatter plots #
# annual & 5 year maps   #
#~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())
library(ini, lib = '/share/homes/annieb6/temp_packages/')
library(ggplot2)
library(raster)
library(gridExtra)
library(latticeExtra)

source('/share/code/st_gpr/central/stgpr/r_functions/utilities/utility.r')

RMSE = function(m, o){
    sqrt(mean((m - o)^2))
  }

# Which run ID?
run_id           <- 174266
antimicrobial    <- 'FQNS'
species          <- 'Paratyphi'

#read in models results
# h5ls("/ihme/covariates/ubcov/model/output/61448/output_0_0.h5")
gpr <- model_load(run_id, 'gpr')
st <- model_load(run_id, 'st')
st$st <- inv.logit(st$st)
lm <- model_load(run_id, 'stage1')
lm$stage1 <- inv.logit(lm$stage1)


#join the data together
mydata <- merge(gpr, st, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'))
mydata <- merge(mydata, lm, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'))

#read in input data
params <- model_load(run_id, 'parameters')
input_data <- read.csv(params$path_to_data)
input_data <- input_data[input_data$is_outlier == 0,]
rm(params, lm, st)

#pull in IHME locations shapefile
endemic_locs <- read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/typhi_endemic_locations2.csv', stringsAsFactors = F)
endemic_locs  <- endemic_locs[c("loc_id", "loc_name", "spr_reg_id", "region_id", 'level', 'ihme_lc_id')]
endemic_locs <- endemic_locs[endemic_locs$region_id != 65,]
if(species=='Paratyphi'){ 
   endemic_locs <- endemic_locs[endemic_locs$spr_reg_id != 166,]
   endemic_locs <- endemic_locs[endemic_locs$spr_reg_id != 137,]
   
}

#merge location names onto the data and restrict to locations in the endemic region
mydata <- mydata[mydata$location_id %in% endemic_locs$loc_id,]
mydata <- merge(mydata, endemic_locs, by.x = 'location_id', by.y = 'loc_id')
gpr <- gpr[gpr$location_id %in% endemic_locs$loc_id,]
gpr <- merge(gpr, endemic_locs, by.x = 'location_id', by.y = 'loc_id')

#calculate confidence intervals for the input data
input_data$upper_ci <- input_data$val+(1.96*sqrt(input_data$var))
input_data$lower_ci <- input_data$val-(1.96*sqrt(input_data$var))
input_data$lower_ci[input_data$lower_ci<0] <- 0
input_data$upper_ci[input_data$upper_ci>1] <- 1
input_data <- input_data[c("location_id", "year_id", 'age_group_id', 'sex_id', "val", "upper_ci", "lower_ci")] 

#merge input data onto output data
mydata <- merge(input_data, mydata, by = c('location_id', 'year_id', 'age_group_id', 'sex_id'), all.x = T, all.y = T)

#order data by alphabetical country and region
mydata <- mydata[order(mydata$spr_reg_id, mydata$region_id, mydata$ihme_lc_id),]
mydata <- mydata[!is.na(mydata$gpr_mean),]

#save output data
write.csv(gpr, paste0('/ihme/covariates/ubcov/model/output/', run_id, '/model_estimates.csv'), row.names = F)

#plot the data in scatterplots
#split into national and subnational plots
national <- mydata[mydata$level == 3,]
subnational <- mydata[mydata$level == 4,]

pdf(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/National_GPR_time_series.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(national$region_id))){
  subset <- national[national$region_id == unique(national$region_id)[i],]
 print(
   ggplot(subset)+
      geom_line(aes(x=year_id, y = gpr_mean),color = 'green')+
      geom_ribbon(aes(ymin = gpr_lower, ymax=gpr_upper, x = year_id), alpha = 0.1, fill = 'green') +
      geom_line(aes(x=year_id, y = st), color = 'blue')+
      geom_line(aes(x=year_id, y = stage1), color = 'red')+
      geom_pointrange(aes(x=year_id, y = val, ymin = lower_ci, ymax = upper_ci)) +
      scale_x_continuous("Year", 
                       breaks = seq(1990, 2018, 5),
                       labels = c("1990", "1995", "2000", "2005", "2010", "2015"))+
     ylim(0,1)+
     ylab('Proportion DR')+
     theme_bw()+
     scale_colour_manual(name = 'Model', values=vars)+
     theme(legend.position = "bottom")+
     ggtitle(unique(subset$region_id))+      
     facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$location_id)))))+
     theme(axis.text.x = element_text(angle = 90, hjust = 1))+
     theme(plot.title = element_text(hjust = 0.5))
 )
}
dev.off()

# #get the coutry for subnational estimates
# subnational$country <- substr(subnational$ihme_lc_id, 1, 3)
# 
# pdf(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/Subnational_GPR_time_series.pdf'),
#     height = 8.3, width = 11.7)
# 
# for(i in 1:length(unique(subnational$country))){
#   subset <- subnational[subnational$country == unique(subnational$country)[i],]
#   print(
#     ggplot(subset)+
#       geom_line(aes(x=year_id, y = gpr_mean), color = 'green')+
#       geom_ribbon(aes(ymin = gpr_lower, ymax=gpr_upper, x = year_id), alpha = 0.1, fill = 'green') +
#       geom_line(aes(x=year_id, y = st), color = 'blue')+
#       geom_line(aes(x=year_id, y = stage1), color = 'red')+
#       geom_pointrange(aes(x=year_id, y = data, ymin = lower_ci, ymax = upper_ci)) +
#       scale_x_continuous("Year", 
#                          breaks = seq(1990, 2018, 5),
#                          labels = c("1990", "1995", "2000", "2005", "2010", "2015"))+
#       ylim(0,1)+
#       ylab('Proportion DR')+
#       theme_bw()+
#      # theme(legend.position = "none")+
#       facet_wrap(~loc_name, nrow = ceiling(sqrt(length(unique(subset$location_id)))))+
#       ggtitle(unique(subset$country))+     
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(plot.title = element_text(hjust = 0.5))
#   )
# }
# dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~#
# plot maps for each year #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
library(sf)
library(viridis)
IHME_shp <- st_read('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.shp')

#select only national locations
IHME_shp <- IHME_shp[IHME_shp$level ==3,] 

endemic_shp <- IHME_shp[IHME_shp$loc_id %in% endemic_locs$loc_id,] 

background <- st_read('/snfs1/WORK/11_geospatial/admin_shapefiles/00_raw_gaul/lbd_standard_admin_0.shp')

# #get rid of nationals which have subnationals
# endemic_shp <- endemic_shp[endemic_shp$level !=5,] #drop level 5 india
# endemic_shp <- endemic_shp[endemic_shp$level !=0,] #dont know why some Kenyas have level 0
# endemic_shp <- endemic_shp[!(endemic_shp$loc_id%in%endemic_shp$parent_id),]

#reshape estimates to wide
mydata <- data.table(gpr)
#limit to national estimates
mydata <- mydata[mydata$level == 3,]

#merge the data onto the shapefile
estimates <- merge(endemic_shp, mydata, by.x = 'loc_id', by.y = 'location_id', all.x = T, all.y = T)

## Plot 5 year estimates 
png(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/5year_estimates_maps.png'),
    height = 30, width = 20, units = 'cm', res = 300)
     ggplot()+
       geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
       geom_sf(data = estimates[estimates$year_id == 1990 |
                                  estimates$year_id == 1995 |
                                  estimates$year_id == 2000 |
                                  estimates$year_id == 2005 |
                                  estimates$year_id == 2010 |
                                  estimates$year_id == 2015,], aes(fill = gpr_mean),colour = 'black', size = 0.15)+
       theme_bw()+
       theme(line = element_blank(),
             axis.text = element_blank())+
       theme(plot.title = element_text(hjust = 0.5))+
       scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 1))+
       labs(fill = paste(antimicrobial, species, '(proportion)', sep = ' '))+
       facet_wrap(~year_id, ncol = 2)+
       # xlim(-20,150)+
       # ylim(-35,55)
       xlim(30, 150)+
       ylim(-12,55)
     dev.off()
     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# plot comparison between in sample fits and data #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

mydata <- national[!is.na(national$val),]

png(paste0('/ihme/covariates/ubcov/model/output/', run_id, '/in_sample_correlation.png'),
    height = 12, width = 12, res = 350, unit = 'in')
print(
ggplot(mydata)+
  geom_point(aes(x = val, y=gpr_mean))+
  xlim(0, 1)+
  ylim(0, 1)+
  geom_abline(slope = 1, intercept = 0, colour = 'red')+
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  labs(x = "Data Estimate",
    y = "Mean Prediction",
    size = "Weight",
    title = "IS Validation Plot for Stacked Ensemble-STGPR",
    subtitle = paste0("RMSE = ", round(RMSE(mydata$gpr_mean, mydata$val),2), "; R2 = ", round(cor(mydata$val, mydata$gpr_mean)^2,2)))
)

dev.off()

