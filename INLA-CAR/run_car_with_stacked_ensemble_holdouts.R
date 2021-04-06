
##############################
#### set working directory####
##############################
rm(list = ls())
setwd("Z:/AMR/Pathogens/typhi_paratyphi/model_results/CAR_INLA")
model_id <-  '9_stacked_ensemble_holdouts'
dir.create(model_id)
#######################
#### Load libraries####
#######################
library(plyr)
library(spData)
library(spdep)
library(CARBayesST)
library(maptools)
library(INLA)
library(raster)
library(ggplot2)
library(data.table)
library(foreign)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
######################################
#### Creating neigbourhood matrix ####
######################################

#Load Country Shapefile
# units <- st_read('Z:/AMR/Shapefiles/typhi_endemic.shp')
# 
# #Create adjacency matrix
# shp.adj <- poly2nb(units,queen=TRUE, row.names=units$adj_id)
# 
# #Convert the adjacency matrix into a file in the INLA format and save
# nb2INLA("typhi_adj.adj", shp.adj)

#Visualize the graph and get summary 
g<-inla.read.graph(filename ="typhi_adj.adj")

## Model formula####

formula <-  as.formula('pos ~ -1 + gam + nnet + rf +
f(reg0, model="bym2", graph="typhi_adj.adj")+
f(time0,model="rw1")+
f(reg1,time2,model="iid")')

##########################
####  Load Input Data ####
##########################
#Read in the data
master_data <- fread(paste0(model_id, "/master_data.csv"))

#calculate number resistance
master_data <- master_data[,.(location_id, year_id, nid, 
                              pos = round(val*sample_size,0), 
                              sample_size, val, master_fold_id)]

#get the adjID from the shapefile
locs <- read.dbf('Z:/AMR/Shapefiles/typhi_endemic.dbf')
locs <- data.frame(locs[c('loc_id', 'adj_id', 'ihme_lc_id', 'region_id')])
locs$loc_id <- as.numeric(as.character(locs$loc_id))
locs$adj_id <- as.numeric(as.character(locs$adj_id))
locs$region_id <- as.numeric(as.character(locs$region_id))

#merge locs onto data
master_data <- merge(master_data, locs, by.x = c('location_id'), by.y = c('loc_id'))

for(i in 1:5){
  covs <- fread(paste0("9_stacked_ensemble_holdouts/stackers_", i, ".csv"))
  covs$year_id <-  covs$year_id-2
  covs <- merge(covs, locs, by.x = 'location_id', by.y = 'loc_id')
  covs <- covs[order(covs$year_id, covs$adj_id),]
  
  #drop the data from the holdout
  mydata <- master_data 
  mydata$pos[mydata$master_fold_id == i] <- NA
  mydata$sample_size[mydata$master_fold_id == i] <- NA
  mydata$val[mydata$master_fold_id == i] <- NA
  
  #add the covariates onto the data
  mydata <- merge(covs, mydata, by = c('location_id', 'year_id', 'adj_id', 'ihme_lc_id', 'region_id'))
  
  #Make year id 1:29
  mydata$year <- mydata$year_id-1989
  
  mydata <- cbind(mydata, reg0=mydata$adj_id, reg1=mydata$adj_id, time0=mydata$year, time1=mydata$year, time2=mydata$year)
  
  typhi.INLA <- inla(formula, family = "binomial", Ntrials = sample_size, data = mydata, 
                   control.compute = list(cpo = T, dic = T), control.inla = list(strategy = "laplace"),
                   control.predictor = list(link=1),
                   control.family=list(link="logit"),
                   verbose = TRUE) 

  #get OOS preds
  master_data[, paste0('holdout_', i) := typhi.INLA$summary.fitted.values$mean]
  master_data[, paste0('lower_HO', i) := typhi.INLA$summary.fitted.values$`0.025quant`]
  master_data[, paste0('upper_HO', i) := typhi.INLA$summary.fitted.values$`0.975quant`]
}
  
#calculate metrics
master_data$oos <- NA
master_data$oos[master_data$master_fold_id==1] <- master_data$holdout_1[master_data$master_fold_id==1]
master_data$oos[master_data$master_fold_id==2] <- master_data$holdout_2[master_data$master_fold_id==2]
master_data$oos[master_data$master_fold_id==3] <- master_data$holdout_3[master_data$master_fold_id==3]
master_data$oos[master_data$master_fold_id==4] <- master_data$holdout_4[master_data$master_fold_id==4]
master_data$oos[master_data$master_fold_id==5] <- master_data$holdout_5[master_data$master_fold_id==5]
# master_data$oos[master_data$master_fold_id==6] <- master_data$holdout_6[master_data$master_fold_id==6]
# master_data$oos[master_data$master_fold_id==7] <- master_data$holdout_7[master_data$master_fold_id==7]
# master_data$oos[master_data$master_fold_id==8] <- master_data$holdout_8[master_data$master_fold_id==8]
# master_data$oos[master_data$master_fold_id==9] <- master_data$holdout_9[master_data$master_fold_id==9]
# master_data$oos[master_data$master_fold_id==10] <- master_data$holdout_10[master_data$master_fold_id==10]

master_data$lower <- NA
master_data$lower[master_data$master_fold_id==1] <- master_data$lower_HO1[master_data$master_fold_id==1]
master_data$lower[master_data$master_fold_id==2] <- master_data$lower_HO2[master_data$master_fold_id==2]
master_data$lower[master_data$master_fold_id==3] <- master_data$lower_HO3[master_data$master_fold_id==3]
master_data$lower[master_data$master_fold_id==4] <- master_data$lower_HO4[master_data$master_fold_id==4]
master_data$lower[master_data$master_fold_id==5] <- master_data$lower_HO5[master_data$master_fold_id==5]
# master_data$lower[master_data$master_fold_id==6] <- master_data$lower_HO6[master_data$master_fold_id==6]
# master_data$lower[master_data$master_fold_id==7] <- master_data$lower_HO7[master_data$master_fold_id==7]
# master_data$lower[master_data$master_fold_id==8] <- master_data$lower_HO8[master_data$master_fold_id==8]
# master_data$lower[master_data$master_fold_id==9] <- master_data$lower_HO9[master_data$master_fold_id==9]
# master_data$lower[master_data$master_fold_id==10] <- master_data$lower_HO10[master_data$master_fold_id==10]

master_data$upper <- NA
master_data$upper[master_data$master_fold_id==1] <- master_data$upper_HO1[master_data$master_fold_id==1]
master_data$upper[master_data$master_fold_id==2] <- master_data$upper_HO2[master_data$master_fold_id==2]
master_data$upper[master_data$master_fold_id==3] <- master_data$upper_HO3[master_data$master_fold_id==3]
master_data$upper[master_data$master_fold_id==4] <- master_data$upper_HO4[master_data$master_fold_id==4]
master_data$upper[master_data$master_fold_id==5] <- master_data$upper_HO5[master_data$master_fold_id==5]
# master_data$upper[master_data$master_fold_id==6] <- master_data$upper_HO6[master_data$master_fold_id==6]
# master_data$upper[master_data$master_fold_id==7] <- master_data$upper_HO7[master_data$master_fold_id==7]
# master_data$upper[master_data$master_fold_id==8] <- master_data$upper_HO8[master_data$master_fold_id==8]
# master_data$upper[master_data$master_fold_id==9] <- master_data$upper_HO9[master_data$master_fold_id==9]
# master_data$upper[master_data$master_fold_id==10] <- master_data$upper_HO10[master_data$master_fold_id==10]

coverage <- master_data$val[!is.na(master_data$val)]>master_data$lower[!is.na(master_data$val)] & master_data$val[!is.na(master_data$val)]<master_data$upper[!is.na(master_data$val)]

model_metrics <- data.frame(r2 = cor(master_data$val[!is.na(master_data$val)], master_data$oos[!is.na(master_data$val)])^2,
                            RMSE = RMSE(master_data$val[!is.na(master_data$val)], master_data$oos[!is.na(master_data$val)]),
                            coverage = length(coverage[coverage==TRUE])/length(coverage)*100)

write.csv(model_metrics, paste0(model_id, '/model_OOS_metrics.csv'), row.names = F)


png(paste0(model_id, '/OOS_plot.png'),
    height = 20, width = 20, units = 'cm', res = 200)
ggplot(master_data, aes(x = val, y = oos, size = sample_size))+
  geom_point()+
  xlim(0,1)+
  ylim(0,1)+
  geom_abline(slope = 1, intercept = 0, colour = 'red')+
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  labs(
    x = "Data Estimate",
    y = "Mean Prediction",
    size = "Weight")  

dev.off()


