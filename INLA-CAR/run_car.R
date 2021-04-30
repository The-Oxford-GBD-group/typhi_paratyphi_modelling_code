##############################
#### set working directory####
##############################
rm(list = ls())
setwd("Z:/AMR/Pathogens/typhi_paratyphi/model_results/CAR_INLA")
model_id <-  '20_sp_precPrior'
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
library(foreign)
library(plyr)
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
mydata <- read.csv("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/outliered/MDR_typhi_outliered_over5.csv") 
mydata <- mydata[mydata$is_outlier == 0,]

#merge locs onto data
mydata <- merge(mydata, locs, by.x = c('location_id'), by.y = c('loc_id'))

#calculate number resistance
mydata$pos <- round(mydata$val*mydata$sample_size, 0)

#read in covariates, convert location and year IDs and centre scale
covs <- read.csv("child_models.csv")
# covs <- read.csv('Z:/AMR/Pathogens/typhi_paratyphi/covariates/cleaned_covs.csv')

#select those to include
covs_to_include <- c('gam', 'rf', 'nnet')

# covs_to_include <-  c("cv_anc4_coverage_prop",
#                       "cv_dtp3_coverage_prop",
#                       "cv_hospital_beds_per1000",
#                       "cv_mean_temperature",
#                       "cv_sanitation_prop",
#                       "cv_antimalarial_effective_tmt_map",
#                       "cv_stunting_prop_haz_under_2sd",
#                       'cv_intest_typhoid',
#                       "J01C",
#                       'QA',
#                       "ddd_per_1000", "cv_tfr")

#restrict covs to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']

covs$year_id <-  covs$year_id-2
covs <- merge(covs, locs, by.x = 'location_id', by.y = 'loc_id')
covs <- covs[order(covs$year_id, covs$adj_id),]

# #centre scale the covariates
# covs <- data.frame(covs)
# covs[colnames(covs) %in% covs_to_include] <- scale(covs[colnames(covs) %in% covs_to_include])
# mydata$QA <-  scale(mydata$QA)

#add the covariates onto the data
mydata <- merge(covs, mydata)

#add the covs data frame onto the data for predictions
# This will mean the data for fitting is on top and prediction on the bottom
mydata <- rbind.fill(mydata, covs)

# mydata$QA[is.na(mydata$QA)] <-  min(mydata$QA)

#recalculate the location ID and year ID to be 1:n
mydata$year <- mydata$year_id-1989


#Introduce a columns  space and time
mydata <- cbind(mydata, reg0=mydata$adj_id, reg1=mydata$adj_id, time0=mydata$year, time1=mydata$year, time2=mydata$year, reg_id = mydata$region_id)
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
prec_prior <-  list(prec = list(prior = "pc.prec", param = c(1, 0.01)), initial = 4, fixed = FALSE)  #theta1
phi_prior <-   list(prec = list(prior = "pc", param = c(0.5, 0.5)), initial = -3, fixed = FALSE)  #theta2

formula <-  as.formula(paste0('pos ~ -1 + ', paste(covs_to_include, collapse = " + "), ' +
f(reg0, model="bym2", graph="typhi_adj.adj", hyper=list(prec_prior, phi_prior))+
f(time0,model="rw1")+
f(reg1,time2,model="iid")'))

# +f(survey, model="iid")'))

## Type I interaction
#f(reg1,time1,model="iid")

## Type II interaction
#f(reg1, model="iid", group = time1, control.group=list(model="rw2"))

## Type III interaction
#f(time1, model="iid", group =reg1,control.group=list(model="besag", graph = "typhi_adj.adj"))

## Type IV interaction
#f(reg1,model = "besag", graph = "typhi_adj.adj",group = time1, control.group = list(model = "rw2"))

typhi.INLA <- inla(formula, family = "binomial", Ntrials = sample_size, data = mydata, 
                                    control.compute = list(cpo = T, dic = T), control.inla = list(strategy = "laplace"),
						control.predictor = list(link=1),
						control.family=list(link="logit"),
						verbose = TRUE) 

summary(typhi.INLA)
saveRDS(typhi.INLA, paste0(model_id, '/full_model.rds'))

#get insample preds
start <- length(mydata$val[!is.na(mydata$val)])+1
end <- length(mydata$val)
predictedmean<-typhi.INLA$summary.fitted.values$mean[start:end]
predictedsd<-typhi.INLA$summary.fitted.values$sd[start:end]
predictedci97.5<-typhi.INLA$summary.fitted.values$`0.975quant`[start:end]
predictedci2.5<-typhi.INLA$summary.fitted.values$`0.025quant`[start:end]
typhi.df<- data.frame(location_id = mydata$location_id[is.na(mydata$val)],
                      adj_id = mydata$adj_id[is.na(mydata$val)],
                      year_id = mydata$year_id[is.na(mydata$val)], 
                      predictedmean, predictedsd,predictedci2.5, predictedci97.5)

typhi.df <-  merge(typhi.df, locs, by = 'adj_id')

write.csv(typhi.df, paste0(model_id, "/typhi_fitted_final.csv"),row.names=F)

#calculate metrics
results <- mydata[!is.na(mydata$pos),]
results <- merge(results, typhi.df, all.y = T)

coverage <- results$val[!is.na(results$val)]>results$predictedci2.5[!is.na(results$val)] & results$val[!is.na(results$val)]<results$predictedci97.5[!is.na(results$val)]

model_metrics <- data.frame(r2 = cor(results$val[!is.na(results$val)], results$predictedmean[!is.na(results$val)])^2,
                            RMSE = RMSE(results$val[!is.na(results$val)], results$predictedmean[!is.na(results$val)]),
                            coverage = length(coverage[coverage==TRUE])/length(coverage)*100)

write.csv(model_metrics, paste0(model_id, '/model_metrics.csv'), row.names = F)

#plot out estimates

pdf(paste0(model_id, '/estimates.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(results$region_id))){
  subset <- results[results$region_id == unique(results$region_id)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year_id, y = predictedmean),color = 'green')+
      geom_ribbon(aes(ymin = predictedci2.5, ymax=predictedci97.5, x = year_id), alpha = 0.1, fill = 'green') +
      geom_point(aes(x = year_id, y = val))+
      # geom_pointrange(aes(x=year_id, y = val, ymin = lower_ci, ymax = upper_ci)) +
      scale_x_continuous("Year", 
                         breaks = seq(1990, 2018, 5),
                         labels = c("1990", "1995", "2000", "2005", "2010", "2015"))+
      ylim(0,1)+
      ylab('Proportion DR')+
      theme_bw()+
      theme(legend.position = "bottom")+
      ggtitle(unique(subset$region_id))+      
      facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$adj_id)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))
  )
}
dev.off()

