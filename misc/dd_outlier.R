#Set working directory
rm(list = ls())

#Load required packages
library("DDoutlier")
library("scales")
library("ggrepel")

#Read in the mydata
mydata<-read.csv('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/clean_data/outliered/MDR_typhi_outliered_over5.csv')
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
               # 'ln_ddd_per_1000_raw',
               # 'cv_hib3_coverage_prop',
               # "cv_hospital_beds_per1000",
               # "cv_mean_temperature",
               # "cv_pollution_outdoor_pm25",
               # "cv_sanitation_prop",
               # 'cv_intest_paratyph',
               # "J01C",
               # "cv_hiv",
               # 'cv_tfr',
               # 'cv_he_cap',
               'location_id')]

df <- df[complete.cases(df),]
rownames(df) <- c()

head(df)

##Natural Neighbor (NAN) algorithm to return the self-adaptive neighborhood

K <- NAN(df, NaN_Edges=FALSE)$r
# K<-7

##Connectivity-based Outlier Factor (COF) algorithm
outlier_score_COF <- COF(dataset=df, k=K)

##Influenced Outlierness (INFLO) algorithm: greater INFLO = greater outlierness
outlier_score_INFLO <- INFLO(dataset=df, k=K)

##Kernel Density Estimation Outlier Score (KDEOS) algorithm with gaussian kernel: KDEOS scores normalized between 1 and 0, with 1 being the greatest outlierness
outlier_score_KDEOS <- KDEOS(dataset=df, k_min=K, k_max=15)

##Aggregated k-nearest neighbors distance over different k’s: greater distance= greater outlierness
outlier_score_KNN_AGG <- KNN_AGG(dataset=df, k_min=K, k_max=15)

##In-degree for observations in a k-nearest neighbors graph
outlier_score_KNN_IN <- KNN_IN(dataset=df, k=K)

##Sum of distance to k-nearest neighbors
outlier_score_KNN_SUM <- KNN_SUM(dataset=df, k=K)

##Local Density Factor (LDF) algorithm with gaussian kernel
outlier_score_LDF <- LDF(dataset=df, k=K, h=2, c=1)$LDF

##Local Distance-based Outlier Factor (LDOF) algorithm
outlier_score_LDOF <- LDOF(dataset=df, k=K)

##Local Correlation Integral (LOCI) algorithm with constant nearestneighbor parameter
outlier_score_LOCI <- LOCI(dataset=df, alpha=0.5, nn=20, k=K)$norm_MDEF

##Local Outlier Factor (LOF) algorithm
outlier_score_LOF <- LOF(dataset=df, k=K)

##Local Outlier Probability (LOOP) algorithm
outlier_score_LOOP <- LOOP(dataset=df, k=K, lambda=3)

##Natural Outlier Factor (NOF) algorithm
outlier_score_NOF <- NOF(dataset=df)$NOF

##Relative Density-based Outlier Factor (RDOS) algorithm with gaussian kernel
outlier_score_RDOS <- RDOS(dataset=df, k=K, h=2)

##Robust Kernel-based Outlier Factor (RKOF) algorithm with gaussian kernel
outlier_score_RKOF_r <- RKOF(dataset=df, k = K, C = 1, alpha = 1, sigma2 = 1)

##outlier_score_COF_z<-scale(outlier_score_COF)
##outlier_score_INFLO_z<-scale(outlier_score_INFLO)

##Rescale individual algorithm outlier scores on 0 to 1 scale
outlier_score_COF <- rescale(outlier_score_COF)
outlier_score_INFLO <- rescale(outlier_score_INFLO)
outlier_score_KDEOS <- rescale(outlier_score_KDEOS)
outlier_score_KNN_AGG <- rescale(outlier_score_KNN_AGG)
outlier_score_KNN_IN <- rescale(outlier_score_KNN_IN)
outlier_score_KNN_SUM <- rescale(outlier_score_KNN_SUM) 
outlier_score_LDF <- rescale(outlier_score_LDF)
outlier_score_LDOF <- rescale(outlier_score_LDOF)
outlier_score_LOCI <- rescale(outlier_score_LOCI)
outlier_score_LOF <- rescale(outlier_score_LOF)
outlier_score_LOOP <- rescale(outlier_score_LOOP)
outlier_score_NOF<- rescale(outlier_score_NOF)
outlier_score_NOF <- rescale(outlier_score_NOF)
outlier_score_RDOS<- rescale(outlier_score_RDOS)
outlier_score_RKOF_r <- rescale(outlier_score_RKOF_r)

#Calculate overall summed outlier score
outlier_score<-outlier_score_COF+
outlier_score_INFLO+
outlier_score_KDEOS+
outlier_score_KNN_AGG+
outlier_score_KNN_IN+
outlier_score_KNN_SUM+ 
outlier_score_LDF+
outlier_score_LDOF+
outlier_score_LOCI+
outlier_score_LOF+
outlier_score_LOOP+
outlier_score_NOF+
outlier_score_NOF+
outlier_score_RDOS+
outlier_score_RKOF_r

#setting this to outlier 10% as I have done with MAD method
q<-quantile(outlier_score, c(0.90)) 

# df$category[outlier_score< q] <- 0
# df$category[outlier_score>=q] <- 1
# table(df$category)
# 
# head(df)
# #Visualise outliers
# countries_sp <- ggplot(df, aes(x = mydata.year.rescaled, y = mydata.p_resistant)) +
#     geom_point()
# 
# countries_sp +
# geom_label_repel(aes(label = category), size = 3,max.overlaps = 1000)
# 
# ggplot(df, aes(mydata.year.rescaled,mydata.p_resistant, label = category)) +
#   geom_text_repel() +
#   geom_point(color = ifelse(df$category == "", "grey50", "red"))


# my comparison
mydata$dd_outlier <- NA
mydata$dd_outlier[outlier_score< q] <- 0
mydata$dd_outlier[outlier_score>=q] <- 1

pdf('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/outliering/dd_outlier_select_covs_10pc.pdf',
    height = 8.3, width = 11.7)
#plot out a page for each region
for(i in 1:length(unique(mydata$super_region))){
  subset <- mydata[mydata$super_region == unique(mydata$super_region)[i],]
  print(
  ggplot(subset)+
    geom_point(aes(x = year_id, y = p_resistant, colour = as.factor(dd_outlier)))+
    facet_wrap(~country)+
    ylim(0,1)+
    ylab('Proportion DR')+
    theme_bw()+
    scale_x_continuous("Year", 
                       breaks = seq(1990, 2018, 5),
                       labels = c("1990", "1995", "2000", "2005", "2010", "2015"))
  )
}
dev.off()

# pdf('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/outliering/MAD_outlier.pdf',
#     height = 8.3, width = 11.7)
# #plot out a page for each region
# for(i in 1:length(unique(mydata$super_region))){
#   subset <- mydata[mydata$super_region == unique(mydata$super_region)[i],]
#   print(
#     ggplot(subset)+
#       geom_point(aes(x = year_id, y = p_resistant, colour = as.factor(is_outlier)))+
#       facet_wrap(~country)+
#       ylim(0,1)+
#       ylab('Proportion DR')+
#       theme_bw()+
#       scale_x_continuous("Year", 
#                          breaks = seq(1990, 2018, 5),
#                          labels = c("1990", "1995", "2000", "2005", "2010", "2015"))
#   )
# }
# dev.off()
