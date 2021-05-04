##############################
#### set working directory####
##############################
rm(list = ls())
setwd("/ihme/homes/annieb6/AMR/typhi_paratyphi/CAR_INLA/admin1")
model_id <-  '1b_sSA_raw_covs_country_re_int3'
dir.create(model_id)
#######################
#### Load libraries####
#######################
library(data.table)
library(plyr)
library(INLA)
library(ggplot2)
library(foreign)
library(boot)

# 
# library(spdep)
# library(sf)
# 
# library(spData)
# 
# library(CARBayesST)
# library(maptools)
# 
# library(raster)
# 
# library(sf)
# library(ggplot2)
# library(viridis)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~#
# Read in data & covs ####
#~~~~~~~~~~~~~~~~~~~~~~~~#
mydata <- fread('MDR_Typhi.csv')
# covs <- read.csv('sSA_child_model_preds.csv')
covs <- read.csv('all_admin1_typhi_covs.csv')
colnames(covs)[colnames(covs) == 'year_id'] <- 'year'

#restrict to sSA
mydata <- mydata[!is.na(mydata$adj_id_sSA),]
covs <- covs[!is.na(covs$adj_id_sSA),]

#add a weights column
mydata$w <- 1
mydata$n <-  round(mydata$n, 0)

# covs_to_include <- c('xgboost', 'gam', 'ridge')
covs_to_include <- c('crutstmp',
                     'nexndvi',
                     'distriverslakes',
                     'intest_typhoid',
                     'physicians_pc',
                     'hospital_beds_per1000',
                     'anc4_coverage_prop',
                     'sanitation_prop'
)

covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='adj_id_sSA' | colnames(covs) =='year'| colnames(covs) == 'COUNTRY_ID']
covs[colnames(covs) %in% covs_to_include] <- data.frame(scale(covs[colnames(covs) %in% covs_to_include]))
# covs$year_scaled <- scale(covs$year)
covs <- data.table(covs)
covs <- na.omit(covs)
covs$iso3 <- as.character(covs$COUNTRY_ID)
covs$COUNTRY_ID <-  NULL


mydata <- merge(mydata, covs, by = c('adj_id_sSA', 'year', 'iso3'))
mydata    <- na.omit(mydata, c('n', 'd', 'p', names(covs)))

#rescale year
mydata$year <- mydata$year-1989
covs$year <- covs$year-1989

#bind the covs onto the mydata as a prediction dataset
mydata <- rbind.fill(mydata, covs)

#Introduce a columns  space and time
mydata <- cbind(mydata, reg0=mydata$adj_id_sSA, reg1=mydata$adj_id_sSA, time0=mydata$year, time1=mydata$year, time2=mydata$year, cntry = as.numeric(as.factor(mydata$iso3)), id = as.numeric(as.factor(mydata$source_id)))
head(mydata)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Fit ST CAR model using INLA ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

formula <-  as.formula(paste0('n ~ -1 + ', paste(covs_to_include, collapse = " + "), ' +
f(reg0, model="bym", graph="typhi_adj_sSA.adj")+
f(time0,model="rw1")+
f(cntry,model = "iid")+
f(id,model = "iid")+
f(reg1, model="iid", group = time1, control.group=list(model="rw2"))'))


# f(reg1,time2,model="iid")'))
# f(reg1, model="bym", graph="typhi_adj_sSA.adj", group = time1, control.group=list(model="rw2"))'))

# f(time1, model="iid", group =reg1,control.group=list(model="besag", graph = "typhi_adj_sSA.adj"))'))

typhi.INLA <- inla(formula, family = "binomial", Ntrials = d, data = mydata, 
                   control.compute = list(cpo = T, dic = T, config = T), control.inla = list(strategy = "laplace"),
                   control.predictor = list(link=1),
                   control.family=list(link="logit"),
                   verbose = TRUE) 

summary(typhi.INLA)
saveRDS(typhi.INLA, paste0(model_id, '/full_model.rds'))
# typhi.INLA <- readRDS(paste0(model_id, '/full_model.rds'))

#~~~~~~~~~~~~~~~~~~~~~~~~#
# Extract predictions ####
#~~~~~~~~~~~~~~~~~~~~~~~~#

#get insample preds
start <- length(mydata$p[!is.na(mydata$p)])+1
end <- length(mydata$p)

predictedmean<-typhi.INLA$summary.fitted.values$mean[start:end]
predictedsd<-typhi.INLA$summary.fitted.values$sd[start:end]
predictedci97.5<-typhi.INLA$summary.fitted.values$`0.975quant`[start:end]
predictedci2.5<-typhi.INLA$summary.fitted.values$`0.025quant`[start:end]

typhi.df<- data.frame(adj_id_sSA = mydata$adj_id_sSA[is.na(mydata$p)],
                      year = mydata$year[is.na(mydata$p)]+1989,
                      predictedmean, predictedsd,predictedci2.5, predictedci97.5)

locs <- fread('all_admin1_typhi_covs.csv')
locs <- unique(locs[,.(COUNTRY_ID, adj_id_sSA)])
typhi.df <-  merge(typhi.df, locs, by = 'adj_id_sSA')

write.csv(typhi.df, paste0(model_id, "/typhi_fitted_final.csv"),row.names=F)

#calculate metrics
results <- mydata[!is.na(mydata$p),]
results <-  results[,1:10]
results$year <- results$year+1989
results <- merge(results, typhi.df, by = c('adj_id_sSA', 'year'),all.x = T, all.y = T)

coverage <- results$p[!is.na(results$p)]>results$predictedci2.5[!is.na(results$p)] & results$p[!is.na(results$p)]<results$predictedci97.5[!is.na(results$p)]

model_metrics <- data.frame(r2 = cor(results$p[!is.na(results$p)], results$predictedmean[!is.na(results$p)])^2,
                            RMSE = RMSE(results$p[!is.na(results$p)], results$predictedmean[!is.na(results$p)]),
                            coverage = length(coverage[coverage==TRUE])/length(coverage)*100,
                            cpo = -mean(log(typhi.INLA$cpo$cpo[!is.na(typhi.INLA$cpo$cpo)])))

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
      geom_line(aes(x=year, y = predictedmean),color = 'green')+
      geom_ribbon(aes(ymin = predictedci2.5, ymax=predictedci97.5, x = year), alpha = 0.1, fill = 'green') +
      geom_point(aes(x = year, y = p))+
      # geom_pointrange(aes(x=year_id, y = val, ymin = lower_ci, ymax = upper_ci)) +
      scale_x_continuous("Year",
                         breaks = seq(1990, 2018, 5),
                         labels = c("1990", "1995", "2000", "2005", "2010", "2015"))+
      ylim(0,1)+
      ylab('Proportion DR')+
      theme_bw()+
      theme(legend.position = "bottom")+
      ggtitle(unique(subset$COUNTRY_ID))+
      facet_wrap(~adj_id_sSA, nrow = ceiling(sqrt(length(unique(subset$adj_id_sSA)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))
  )
}
dev.off()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Get draws and aggregate to country level ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
my_draws <- inla.posterior.sample(100, typhi.INLA)
saveRDS(my_draws, paste0(model_id, '/INLA_draws.rds'))
# my_draws <- readRDS(paste0(model_id, '/INLA_draws.rds'))

pred.names <- rownames(my_draws[[1]]$latent)
pred.names <- pred.names[grepl('Predictor', pred.names)]
pred.names <- pred.names[start:end]


all_pred <- sapply(my_draws, function(x) {
  x$latent[pred.names,]})

all_pred <-  inv.logit(all_pred)
all_pred_dt <-  data.table(all_pred)

#assign location info to this
all_pred_dt$adj_id_sSA <- typhi.df$adj_id_sSA
all_pred_dt$country <- typhi.df$COUNTRY_ID
all_pred_dt$year <- typhi.df$year

# get the population estimates
pop <- fread('all_admin1_typhi_covs.csv')
pop <- pop[,.(adj_id_sSA, year, population)]

all_pred_dt <- merge(all_pred_dt, pop, by = c('adj_id_sSA', 'year'))

# #aggregate up to country level (population weighted mean)
national_ests <- 
  all_pred_dt[, lapply(.SD, weighted.mean, w=population, na.rm=TRUE), by=c('country', 'year'), .SDcols=3:102] 

national_ests$mean <- rowMeans(national_ests[, 3:102])
national_ests$lower <- apply(national_ests[, 3:102], 1, function(x) quantile(x, 0.025))
national_ests$upper <- apply(national_ests[, 3:102], 1, function(x) quantile(x, 0.975))

national_ests <- national_ests[,.(country, year, mean, lower, upper)]

#merge on super region info
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
locs <- locs[c('ihme_lc_id', 'spr_reg_id')]
locs$region[locs$spr_reg_id == 137] <- 'North Africa & Middle East'
locs$region[locs$spr_reg_id == 158] <- 'South sSA'
locs$region[locs$spr_reg_id == 166] <- 'Sub-Saharan Africa'
locs$region[locs$spr_reg_id == 4] <- 'Southeast sSA, East sSA & Oceania'
locs$spr_reg_id <- NULL

national_ests <- merge(national_ests, locs, by.x = 'country', by.y = 'ihme_lc_id')
write.csv(national_ests, paste0(model_id, '/national_estimates.csv'), row.names = F)

#merge on input data
input <- mydata[!is.na(mydata$p),]
input$year <- input$year+1989
input <- input[c('iso3', 'year', 'p')]
national_ests <- merge(national_ests, input, by.x = c('country', 'year'), by.y = c('iso3', 'year'), all.x = T, all.y = T)
national_ests <- national_ests[!is.na(national_ests$region),]

#plot out estimates
pdf(paste0(model_id, '/national_estimates.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(national_ests$region))){
  subset <- national_ests[national_ests$region == unique(national_ests$region)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year, y = mean),color = 'green')+
      geom_ribbon(aes(ymin = lower, ymax=upper, x = year), alpha = 0.1, fill = 'green') +
      geom_point(aes(x = year, y = p))+
      # geom_pointrange(aes(x=year, y = val, ymin = lower_ci, ymax = upper_ci)) +
      scale_x_continuous("Year",
                         breaks = seq(1990, 2018, 5),
                         labels = c("1990", "1995", "2000", "2005", "2010", "2015"))+
      ylim(0,1)+
      ylab('Proportion DR')+
      theme_bw()+
      theme(legend.position = "bottom")+
      ggtitle(unique(subset$region))+
      facet_wrap(~country, nrow = ceiling(sqrt(length(unique(subset$country)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))
  )
}
dev.off()
