##############################
#### set working directory####
##############################
rm(list = ls())
setwd("Z:/AMR/Pathogens/typhi_paratyphi/model_results/CAR_INLA/admin1")
model_id <-  '1_inital_run'
dir.create(model_id)
#######################
#### Load libraries####
#######################
library(data.table)
library(plyr)
library(INLA)


library(spdep)
library(sf)

library(spData)

library(CARBayesST)
library(maptools)

library(raster)
library(ggplot2)
library(sf)
library(ggplot2)
library(foreign)
library(viridis)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in data & covs ####
#~~~~~~~~~~~~~~~~~~~~~~~~#
mydata <- fread('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/MDR_Typhi.csv')
covs <- read.csv('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv')

#add a weights column
mydata$w <- 1
mydata$n <-  round(mydata$n, 0)

covs_to_include <- c('crutstmp',
                     'fertility',
                     'hib3_cov',
                     'sanitation_prop',
                     'distriverslakes',
                     'rqe',
                     'universal_health_coverage',
                     'J01C',
                     'physicians_pc',
                     'hospital_beds_per1000',
                     'anc4_coverage_prop'
)

covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='adj_id' | colnames(covs) =='year']
covs[colnames(covs) %in% covs_to_include] <- data.frame(scale(covs[colnames(covs) %in% covs_to_include]))
# covs$year_scaled <- scale(covs$year)
covs <- data.table(covs)
covs <- na.omit(covs)

mydata <- merge(mydata, covs, by = c('adj_id', 'year'))
mydata    <- na.omit(mydata, c('n', 'd', 'p', names(covs)))

#rescale year
mydata$year <- mydata$year-1989
covs$year <- covs$year-1989

#bind the covs onto the mydata as a prediction dataset
mydata <- rbind.fill(mydata, covs)

#Introduce a columns  space and time
mydata <- cbind(mydata, reg0=mydata$adj_id, reg1=mydata$adj_id, time0=mydata$year, time1=mydata$year, time2=mydata$year)
head(mydata)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Fit ST CAR model using INLA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

formula <-  as.formula(paste0('n ~ -1 + ', paste(covs_to_include, collapse = " + "), ' +
f(reg0, model="bym2", graph="Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/typhi_adj.adj")+
f(time0,model="rw1")+
f(reg1,time2,model="iid")'))

typhi.INLA <- inla(formula, family = "binomial", Ntrials = d, data = mydata, 
                   control.compute = list(cpo = T, dic = T), control.inla = list(strategy = "laplace"),
                   control.predictor = list(link=1),
                   control.family=list(link="logit"),
                   verbose = TRUE) 

summary(typhi.INLA)
saveRDS(typhi.INLA, paste0(model_id, '/full_model.rds'))

#~~~~~~~~~~~~~~~~~~~~~~~~#
# Extract predictions ####
#~~~~~~~~~~~~~~~~~~~~~~~~#
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

#~~~~~~~~~~~~~~~~~~~~~~~#
# Plot out estimates ####
#~~~~~~~~~~~~~~~~~~~~~~~#

#plot the subnat results
#plot out estimates
pdf(paste0(model_id, '/subnational_estimates.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(results$COUNTRY_ID))){
  subset <- results[results$COUNTRY_ID == unique(results$COUNTRY_ID)[i],]
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
      ggtitle(unique(subset$COUNTRY_ID))+      
      facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$adj_id)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))
  )
}
dev.off()

#aggregate up to country level (population weighted mean)
pop <- fread('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv')
pop <- pop[,.(admin_code, year, population)]


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
