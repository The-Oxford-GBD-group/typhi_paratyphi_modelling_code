#### SETUP ####
rm(list = ls())
library(R2WinBUGS)
library(sp)
library(spdep)
library(rgdal)
library(coda)
library(data.table)
library(foreign)
library(tidyverse)
library(plyr)
RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}
setwd("Z:/AMR/Pathogens/typhi_paratyphi")
model_name <- '7_outliers_removed' 
dir.create(paste0('model_results/bugs/', model_name), showWarnings = F)

##Define the model in BUGS language ####
sink("typhi.bug")
cat("
MODEL 
{ 

    for(i in 1:482){
      h.2[i]~dnorm(0,tau.h2)
  }

  	for(i in 1:N){
      h.1[i]~dnorm(0,tau.h1)
  }
    for(i in 1:nTot){
      number_resistant[i] ~ dbin(p[i],sample_size[i])
    logit(p[i])<-beta0+cov1[i]+cov2[i]+cov3[i]+w.1[adj_id[i]]+h.1[adj_id[i]]+theta.1[year[i]]+h.2[source_num[i]]+nu.1[adj_id[i],year[i]]  
  }


#- Space
				
w.1[1:N]~car.normal(adj[],weights[],num[],tau.w)

for(k in 1:sumNumNeigh){
				weights[k]<-1
				}
				
# - Time- RW(1)
				
theta.1[1:T] ~ car.normal(adj.t[], weights.t[], num.t[], tau.t)

             for(t in 1:1) {
                weights.t[t] <- 1;
               adj.t[t] <- t+1;
               num.t[t] <- 1
             }
             for(t in 2:(T-1)) {
                weights.t[2+(t-2)*2] <- 1;
              adj.t[2+(t-2)*2] <- t-1
                weights.t[3+(t-2)*2] <- 1;
               adj.t[3+(t-2)*2] <- t+1;
              num.t[t] <- 2
            }
             for(t in T:T) {
                weights.t[(T-2)*2 + 2] <- 1;
               adj.t[(T-2)*2 + 2] <- t-1;
               num.t[t] <- 1
             }

# Space-time Type I interaction
for( i in 1 : N ) {
for( t in 1 : T ) {
nu.1[i,t]~dnorm(0, tau.nu)
}
}

#Hyper priors

tau.h1~dgamma(0.5,0.0005) 
tau.h2~dgamma(0.5,0.0005) 
tau.t ~ dgamma(0.5,0.0005) 
tau.w ~ dgamma(0.5, 0.0005)
tau.nu ~ dgamma(0.5,0.0005)

#Intercepts
beta0~dflat()

#Coefficients

#for(i in 1:5){ 
#	b[i]~dnorm(0,0.0001) 
#}

}
# end model
", fill=TRUE)
sink()

# Setup the data ####
#get the input data
mydata <- fread("model_prep/clean_data/outliered/MDR_typhi_outliered_over5.csv")
mydata <- mydata[mydata$is_outlier == 0,]
mydata <- mydata[,.(location_id, year_id, nid, 
                    number_resistant = val*sample_size, sample_size, val)]

#get the adjID from the shapefile
locs <- read.dbf('Z:/AMR/Shapefiles/typhi_endemic.dbf')
locs <- data.frame(locs[c('loc_id', 'adj_id', 'ihme_lc_id', 'region_id')])
locs$loc_id <- as.numeric(as.character(locs$loc_id))
locs$adj_id <- as.numeric(as.character(locs$adj_id))
locs$region_id <- as.numeric(as.character(locs$region_id))

#merge locs onto data
mydata <- merge(mydata, locs, by.x = c('location_id'), by.y = c('loc_id'))

#get covs (to have a template for all country-years)
covs <- read.csv("model_results/CAR_INLA/child_models.csv")
covs$year_id <-  covs$year_id-2
covs <- merge(covs, locs, by.x = 'location_id', by.y = 'loc_id')
covs <- covs[order(covs$year_id, covs$adj_id),]

#add the covariates onto the data
mydata <- merge(covs, mydata)

#add the covs data frame onto the data for predictions
# This will mean the data for fitting is on top and prediction on the bottom
mydata <- rbind.fill(mydata, covs)

#Make year id 1:29
mydata$year <- mydata$year_id-1989

# Get the variables required in the model
number_resistant <-  round(mydata$number_resistant,0)
sample_size <-  mydata$sample_size
adj_id <- mydata$adj_id
year <- mydata$year
source_num <- as.numeric(as.factor(mydata$nid))
source_num[is.na(source_num)] <- 482
cov1 <- mydata$nnet
cov2 <- mydata$rf
cov3 <- mydata$gam

#fill in the missing values (made it work with the aggregated file)
sample_size[is.na(sample_size)] <- 50

#set up adjacency matrix info
#Number of neighbours
num<-c(0,1,2,4,0,3,4,3,6,5,0,6,0,8,5,2,6,3,3,0,3,3,2,6,0,5,9,6,6,3,1,6,7,2,
       3,6,3,2,5,4,7,3,7,3,2,1,6,3,0,5,6,4,0,1,7,4,3,3,5,3,4,1,0,3,1,0,1,4,
       0,1,1,2,3,2,2,1,1,7,3,2,6,4,4,0,2,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

sumNumNeigh<-sum(num)

#specify data and adjacency matrix
bugs.data <- list(N=102,T=29,nTot=length(mydata$number_resistant),
                  number_resistant=number_resistant,sample_size=sample_size,
                  adj_id = adj_id, year = year, source_num = source_num,
                  cov1 = cov1, cov2 = cov2, cov3 = cov3,
                  num=num,sumNumNeigh=sumNumNeigh,
                  adj=c(
                    
                    3,
                    2,68,
                    6,9,14,16,
                    
                    4,9,14,
                    14,15,21,27,
                    10,17,19,
                    4,6,14,16,27,30,
                    8,12,14,15,17,
                    
                    10,15,17,27,29,33,
                    
                    4,6,7,9,10,15,21,27,
                    7,10,12,14,27,
                    4,9,
                    8,10,12,18,19,33,
                    17,19,33,
                    8,17,18,
                    
                    7,14,27,
                    23,26,28,
                    22,28,
                    28,29,32,33,40,41,
                    
                    22,27,28,29,30,
                    7,9,12,14,15,21,26,29,30,
                    22,23,24,26,29,40,
                    12,24,26,27,28,33,
                    9,26,27,
                    36,
                    24,33,34,35,36,41,
                    12,17,18,24,29,32,35,
                    32,36,
                    32,33,75,
                    31,32,34,41,42,43,
                    48,51,52,
                    44,47,
                    42,43,45,46,47,
                    24,28,41,52,
                    24,32,36,40,43,51,52,
                    36,39,43,
                    36,39,41,42,47,50,51,
                    38,47,50,
                    39,47,
                    39,
                    38,39,43,44,45,50,
                    37,50,51,
                    
                    43,44,47,48,51,
                    37,41,43,48,50,52,
                    37,40,41,51,
                    
                    55,
                    54,56,58,59,67,70,72,
                    55,59,61,71,
                    58,59,61,
                    55,57,59,
                    55,56,57,58,61,
                    61,62,64,
                    56,57,59,60,
                    60,
                    
                    60,65,86,
                    64,
                    
                    55,
                    3,72,79,81,
                    
                    55,
                    56,
                    55,68,
                    78,80,85,
                    78,81,
                    35,82,
                    83,
                    78,
                    73,74,77,80,81,82,85,
                    68,81,83,
                    73,78,
                    68,74,78,79,82,83,
                    75,78,81,83,
                    76,79,81,82,
                    
                    73,78,
                    64
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                    
                  ))

##Load initial values

inits <- function(){
  list(beta0=rnorm(1), tau.h1=runif(1,0,2),tau.h2=runif(1,0,2), tau.t=runif(1,0,2), tau.w=runif(1,0,2), tau.nu=runif(1,0,2))
}

inits()

#Paramters to estimate and keep track of
parameters <- c("beta0","tau.h1","tau.h2","tau.t","tau.w","tau.nu","p")

# MCMC settings
nchains <- 1
niter <- 10000
nburnin <- 5000
nthin <- 1

##Locate WinBUGS by setting path below specifically for the computer used.
bugs.dir<-"C:/Users/annieb/Documents/WinBUGS14"

# Do the MCMC stuff from R
out <- bugs(data = bugs.data, inits = inits, parameters.to.save = parameters, model.file = "typhi.bug", n.chains = nchains, n.thin=nthin, n.iter=niter, n.burnin=nburnin, debug=TRUE, bugs.directory=bugs.dir)
summary(out)

my_log <- file("my_log.txt")
sink(my_log, append = TRUE, type = "output")
print(out, 3)
closeAllConnections() 

#get the model predictions

start <- length(mydata$number_resistant[!is.na(mydata$number_resistant)])+length(parameters)
end <- length(out$summary[,1])-1
posterior.df <- data.frame(p.mean = out$summary[start:end,1], 
                           p.sd = out$summary[start:end,2],
                           p.lower=out$summary[start:end,3], 
                           p.upper=out$summary[start:end,7],
                           adj_id = adj_id[is.na(number_resistant)],
                           year_id = year[is.na(number_resistant)]+1989)

posterior.df <-  merge(posterior.df, locs, by = 'adj_id')

write.csv(posterior.df,paste0("model_results/bugs/", model_name, "/bugs_final.csv"), row.names=F)

#check in sample stats #### 
results <- mydata[!is.na(mydata$number_resistant),]
results <- merge(results, posterior.df, all.y = T)
coverage <- results$val[!is.na(results$val)]>results$p.lower[!is.na(results$val)] & results$val[!is.na(results$val)]<results$p.upper[!is.na(results$val)]


model_metrics <- data.frame(r2 = cor(results$val[!is.na(results$val)], results$p.mean[!is.na(results$val)])^2,
                            RMSE = RMSE(results$val[!is.na(results$val)], results$p.mean[!is.na(results$val)]),
                            coverage = length(coverage[coverage==TRUE])/length(coverage)*100)

write.csv(model_metrics, paste0('model_results/bugs/', model_name, '/model_metrics.csv'), row.names = F)

#Plot out model estimates ####
results$region_id <- as.character(results$region_id)
results$ihme_lc_id <- as.character(results$ihme_lc_id)
results <- results[!is.na(results$region_id),]

pdf(paste0('model_results/bugs/', model_name, '/estimates.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(results$region_id))){
  subset <- results[results$region_id == unique(results$region_id)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year_id, y = p.mean),color = 'green')+
      geom_ribbon(aes(ymin = p.lower, ymax=p.upper, x = year_id), alpha = 0.1, fill = 'green') +
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

