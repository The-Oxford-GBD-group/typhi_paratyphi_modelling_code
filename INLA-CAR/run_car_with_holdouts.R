
##############################
#### set working directory####
##############################
rm(list = ls())
setwd("Z:/AMR/Pathogens/typhi_paratyphi/model_results/CAR_INLA")
model_id <-  '11a_basic_5_holdouts'
dir.create(model_id)
#######################
#### Load libraries####
#######################

library(spData)
library(spdep)
library(CARBayesST)
library(maptools)
library(INLA)
library(raster)
library(ggplot2)
library(data.table)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
######################################
#### Creating neigbourhood matrix ####
######################################

#Load Country Shapefile
# units <- st_read('Z:/AMR/Shapefiles/typhi_endemic.shp')

# #Create adjacency matrix 
# shp.adj <- poly2nb(units,queen=TRUE, row.names=units$idadj) 
# shp.adj
# 
# #Convert the adjacency matrix into a file in the INLA format and save
# nb2INLA("typhi_adj.adj", shp.adj) 

#Visualize the graph and get summary 
g<-inla.read.graph(filename ="typhi_adj.adj")
summary(g)

#get a lookup table of the new loc IDs
locs <- read.dbf('Z:/AMR/Shapefiles/typhi_endemic.dbf')
locs <- data.frame(locs[c('loc_id', 'adj_id', 'ihme_lc_id', 'region_id')])
locs$loc_id <- as.numeric(as.character(locs$loc_id))
locs$adj_id <- as.numeric(as.character(locs$adj_id))
locs$region_id <- as.numeric(as.character(locs$region_id))

##W.list <- nb2listw(temp, style = "B", zero.policy = TRUE)
##W <- nb2mat(temp, style = "C", zero.policy = TRUE)

##########################
####  Load Input Data ####
##########################
#Read in the data
mydata <- fread("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/outliered/MDR_typhi_outliered_over5.csv") 
mydata <- mydata[mydata$is_outlier == 0,]

#set 5 random holdouts
mydata <- mydata[sample(nrow(mydata)),]
mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]

#calculate number resistance
mydata$pos <- round(mydata$val*mydata$sample_size, 0)

#read in covariates, convert location and year IDs and centre scale
covs <- read.csv("Z:/AMR/Pathogens/typhi_paratyphi/covariates/cleaned_covs.csv")

#select those to include
covs_to_include <-  c("cv_anc4_coverage_prop",
                      "cv_dtp3_coverage_prop",
                      "cv_hospital_beds_per1000",
                      "cv_mean_temperature",
                      "cv_sanitation_prop",
                      "cv_antimalarial_effective_tmt_map",
                      "cv_stunting_prop_haz_under_2sd",
                      'cv_intest_typhoid',
                      "J01C",
                      'QA',
                      "ddd_per_1000", "cv_tfr")

#restrict covs to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']

#centre scale the covariates
covs <- data.frame(covs)
covs[colnames(covs) %in% covs_to_include] <- data.frame(scale(covs[colnames(covs) %in% covs_to_include]))
mydata$QA <-  scale(mydata$QA)

#merge covariates onto the data
mydata <- merge(mydata, covs, by = c('location_id', 'year_id'), all.y = F)
mydata$QA[is.na(mydata$QA)] <-  min(mydata$QA)

#recalculate the location ID and year ID to be 1:n
mydata$year <- mydata$year_id-1989
mydata <- merge(mydata, locs, by.x = 'location_id', by.y = 'loc_id')

#check/drop any with NA anywhere imporant
# mydata    <- na.omit(mydata, c('pos', 'sample_size', 'val', names(covs)))

#Introduce a columns  space and time
mydata <- cbind(mydata, reg0=mydata$adj_id, reg1=mydata$adj_id, time0=mydata$year, time1=mydata$year, time2=mydata$year)
head(mydata)

# #add a survey level random effect
# nid <- unique(mydata$nid)
# nid <-  nid[!is.na(nid)]
# nid <- data.frame(nid)
# nid$survey <- 1:length(nid$nid)
# mydata <- merge(mydata, nid, by = 'nid', all.x = T)

#####################################
#### Fit ST CAR model using INLA ####
#####################################

formula <-  as.formula(paste0('pos ~ -1 + ', paste(covs_to_include, collapse = " + "), '+
f(reg0, model="bym2", graph="typhi_adj.adj")+
f(time0,model="rw1")+
f(reg1,time2,model="iid")'))

for(i in 1:5){
  holdout_data <- mydata
  holdout_data$pos[holdout_data$fold_id == i] <- NA
  holdout_data$val[holdout_data$fold_id == i] <- NA
  holdout_data$sample_size[holdout_data$fold_id == i] <- NA
  
  typhi.INLA <- inla(formula, family = "binomial", Ntrials = sample_size, data = holdout_data, 
                   control.compute = list(cpo = T, dic = T), control.inla = list(strategy = "laplace"),
                   control.predictor = list(link=1),
                   control.family=list(link="logit"),
                   verbose = TRUE) 

  #get OOS preds
  mydata[, paste0('holdout_', i) := typhi.INLA$summary.fitted.values$mean]
  mydata[, paste0('lower_HO', i) := typhi.INLA$summary.fitted.values$`0.025quant`]
  mydata[, paste0('upper_HO', i) := typhi.INLA$summary.fitted.values$`0.975quant`]
}
  
#calculate metrics
mydata$oos <- NA
mydata$oos[mydata$fold_id==1] <- mydata$holdout_1[mydata$fold_id==1]
mydata$oos[mydata$fold_id==2] <- mydata$holdout_2[mydata$fold_id==2]
mydata$oos[mydata$fold_id==3] <- mydata$holdout_3[mydata$fold_id==3]
mydata$oos[mydata$fold_id==4] <- mydata$holdout_4[mydata$fold_id==4]
mydata$oos[mydata$fold_id==5] <- mydata$holdout_5[mydata$fold_id==5]
# mydata$oos[mydata$fold_id==6] <- mydata$holdout_6[mydata$fold_id==6]
# mydata$oos[mydata$fold_id==7] <- mydata$holdout_7[mydata$fold_id==7]
# mydata$oos[mydata$fold_id==8] <- mydata$holdout_8[mydata$fold_id==8]
# mydata$oos[mydata$fold_id==9] <- mydata$holdout_9[mydata$fold_id==9]
# mydata$oos[mydata$fold_id==10] <- mydata$holdout_10[mydata$fold_id==10]


mydata$lower <- NA
mydata$lower[mydata$fold_id==1] <- mydata$lower_HO1[mydata$fold_id==1]
mydata$lower[mydata$fold_id==2] <- mydata$lower_HO2[mydata$fold_id==2]
mydata$lower[mydata$fold_id==3] <- mydata$lower_HO3[mydata$fold_id==3]
mydata$lower[mydata$fold_id==4] <- mydata$lower_HO4[mydata$fold_id==4]
mydata$lower[mydata$fold_id==5] <- mydata$lower_HO5[mydata$fold_id==5]
# mydata$lower[mydata$fold_id==6] <- mydata$lower_HO6[mydata$fold_id==6]
# mydata$lower[mydata$fold_id==7] <- mydata$lower_HO7[mydata$fold_id==7]
# mydata$lower[mydata$fold_id==8] <- mydata$lower_HO8[mydata$fold_id==8]
# mydata$lower[mydata$fold_id==9] <- mydata$lower_HO9[mydata$fold_id==9]
# mydata$lower[mydata$fold_id==10] <- mydata$lower_HO10[mydata$fold_id==10]

mydata$upper <- NA
mydata$upper[mydata$fold_id==1] <- mydata$upper_HO1[mydata$fold_id==1]
mydata$upper[mydata$fold_id==2] <- mydata$upper_HO2[mydata$fold_id==2]
mydata$upper[mydata$fold_id==3] <- mydata$upper_HO3[mydata$fold_id==3]
mydata$upper[mydata$fold_id==4] <- mydata$upper_HO4[mydata$fold_id==4]
mydata$upper[mydata$fold_id==5] <- mydata$upper_HO5[mydata$fold_id==5]
# mydata$upper[mydata$fold_id==6] <- mydata$upper_HO6[mydata$fold_id==6]
# mydata$upper[mydata$fold_id==7] <- mydata$upper_HO7[mydata$fold_id==7]
# mydata$upper[mydata$fold_id==8] <- mydata$upper_HO8[mydata$fold_id==8]
# mydata$upper[mydata$fold_id==9] <- mydata$upper_HO9[mydata$fold_id==9]
# mydata$upper[mydata$fold_id==10] <- mydata$upper_HO10[mydata$fold_id==10]

coverage <- mydata$val[!is.na(mydata$val)]>mydata$lower[!is.na(mydata$val)] & mydata$val[!is.na(mydata$val)]<mydata$upper[!is.na(mydata$val)]

model_metrics <- data.frame(r2 = cor(mydata$val[!is.na(mydata$val)], mydata$oos[!is.na(mydata$val)])^2,
                            RMSE = RMSE(mydata$val[!is.na(mydata$val)], mydata$oos[!is.na(mydata$val)]),
                            coverage = length(coverage[coverage==TRUE])/length(coverage)*100)

write.csv(model_metrics, paste0(model_id, '/model_OOS_metrics.csv'), row.names = F)

png(paste0(model_id, '/OOS_plot.png'),
    height = 20, width = 20, units = 'cm', res = 200)
ggplot(mydata, aes(x = val, y = oos, size = sample_size))+
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


