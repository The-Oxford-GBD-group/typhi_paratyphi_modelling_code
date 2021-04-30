rm(list = ls())
#Load required packages
library("DDoutlier")
library("scales")
library("ggrepel")

#Read in the mydata
mydata<-read.csv('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/stgpr_files/MDR_paratyphi.csv')
covs <- read.csv('Z:/AMR/Pathogens/typhi_paratyphi/covariates/cleaned_covs.csv')

mydata <- merge(mydata, covs)
colnames(mydata)[colnames(mydata)=='val'] <- 'p_resistant'

mydata$ln_ddd_per_1000_raw <- log(mydata$ddd_per_1000)

#rescale covs
mydata[,15:73] <- apply(mydata[,15:73],2,function(x) rescale(x))
mydata$year.rescaled<- rescale(mydata$year_id)
mydata$super_region_rescaled <- rescale(as.numeric(as.factor(mydata$super_region)))
mydata$region_rescaled <- rescale(as.numeric(as.factor(mydata$region)))
mydata$location_id <-  rescale(mydata$location_id)
df <- mydata[c('p_resistant',
               'year.rescaled',
               'ln_ddd_per_1000_raw',
               'location_id')]

df <- df[complete.cases(df),]
rownames(df) <- c()

##Natural Neighbor (NAN) algorithm to return the self-adaptive neighborhood
K <- NAN(df, NaN_Edges=FALSE)$r

##Influenced Outlierness (INFLO) algorithm: greater INFLO = greater outlierness
outlier_score_INFLO <- INFLO(dataset=df, k=K)

# rescale outlier scores
outlier_score_INFLO <- rescale(outlier_score_INFLO)

# define outliers in mydata
mydata <- mydata[,1:14]
mydata$is_outlier[outlier_score_INFLO>=quantile(outlier_score_INFLO,.95)] <- 1
mydata$is_outlier[outlier_score_INFLO<quantile(outlier_score_INFLO,.95)] <- 0

#if there are <=2 datapoints per country keep them in
mydata <- data.table(mydata)
pp_country <- mydata[,.(points = .N),
                     by = c('country')] 
pp_country <- pp_country[points <=2,]
mydata$is_outlier[mydata$country %in% pp_country$country] <- 0

#save the data
write.csv(mydata, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/stgpr_files/MDR_paratyphi_outliered_300421.csv', row.names = F)
