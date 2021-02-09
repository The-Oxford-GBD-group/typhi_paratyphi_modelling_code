#~~~~~~~~~~~~~~~~~~~~~~~#
# lm on trimmed data ####
#~~~~~~~~~~~~~~~~~~~~~~~#
library(lme4)
library(caret)
rm(list = ls())

#LM on trimmed data from MR-BRT:
mydata <- data.table(read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/MDR_typhi_outliered_over5.csv', stringsAsFactors = F))
mydata <-  mydata[mydata$sample_size >= 5,]

#load covariates - either centre-scaled or standard
covs <- read.csv('/ihme/homes/annieb6/covariates/cleaned_covs2.csv', stringsAsFactors = F)

covs_to_include <-  c("cv_ddd_per_1000",
                      "cv_J01C")


#restrict covs to those included
covs <- covs[colnames(covs) %in% covs_to_include | colnames(covs)=='location_id' | colnames(covs) =='year_id']
covs <- data.table(covs)
covs <- na.omit(covs)

#merge data and covs
mydata <- merge(mydata, covs, by = c('location_id', 'year_id'), all.x = T, all.y = F)

# transform covs
mydata <- data.frame(mydata)
mydata <- na.omit(mydata)

form <- paste0('logit(val) ~ 1 + ', paste(covs_to_include, collapse = " + "))
form <- as.formula(form)

# model1 <- lm(form, data = mydata)
# 
# summary(model1)
# stepAIC(model1, direction = 'both')

# form <- as.formula(logit(val) ~ 1 + year_id + cv_ldi_pc + cv_pollution_outdoor_pm25 + 
#                      cv_tfr + cv_stunting_prop_haz_under_2sd + J01M)

mydata <- data.table(mydata)
mydata <- mydata[sample(nrow(mydata)),]
mydata[,fold_id := cut(seq(1,nrow(mydata)),breaks=5,labels=FALSE)]
mydata[, a_rowid := seq(1:nrow(mydata))]
mydata <- data.frame(mydata)


train_control_final <- trainControl(method = "cv",
                                    number = 5,
                                    savePredictions = "final",
                                    index = list(mydata$a_rowid[mydata$fold_id!=1],
                                                 mydata$a_rowid[mydata$fold_id!=2],
                                                 mydata$a_rowid[mydata$fold_id!=3],
                                                 mydata$a_rowid[mydata$fold_id!=4],
                                                 mydata$a_rowid[mydata$fold_id!=5]),
                                    indexOut =list(mydata$a_rowid[mydata$fold_id==1],
                                                   mydata$a_rowid[mydata$fold_id==2],
                                                   mydata$a_rowid[mydata$fold_id==3],
                                                   mydata$a_rowid[mydata$fold_id==4],
                                                   mydata$a_rowid[mydata$fold_id==5]))

model_lm<- 
  train(form, data = mydata, method='lm', trControl=train_control_final, tuneLength=3)

mydata$LM <- arrange(model_lm$pred, rowIndex)[,"pred"]
mydata$LM2 <- predict(model_lm, mydata)

mydata$LM <- inv.logit(mydata$LM)
mydata$LM2 <- inv.logit(mydata$LM2)

cor(mydata$val, mydata$LM)^2
RMSE(mydata$val, mydata$LM)

cor(mydata$val, mydata$LM2)^2
RMSE(mydata$val, mydata$LM2)


#try with nested random effects
mydata[,14:16] <-  scale(mydata[,14:16])
mydata$year <- scale(mydata$year_id)
alldata <- mydata

form2 <- paste0('response ~ 1 + ', paste(covs_to_include, collapse = " + "), '+ (1 | super_region/region/country)')
form2 <- as.formula(form2)

mydata <- alldata[alldata$fold_id!=1,]
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model1 <- glmer(form2, data = mydata, family = 'binomial',
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mydata <- alldata[alldata$fold_id!=2,]
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model2 <- glmer(form2, data = mydata, family = 'binomial',
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mydata <- alldata[alldata$fold_id!=3,]
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model3 <- glmer(form2, data = mydata, family = 'binomial',
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mydata <- alldata[alldata$fold_id!=4,]
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model4 <- glmer(form2, data = mydata, family = 'binomial',
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

mydata <- alldata[alldata$fold_id!=5,]
response = cbind(successes = round(mydata$val*mydata$sample_size,0),
                 failures = mydata$sample_size)

model5 <- glmer(form2, data = mydata, family = 'binomial',
                control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

alldata$LMER_ho1 <- inv.logit(predict(model1, alldata, allow.new.levels = T))
alldata$LMER_ho2 <- inv.logit(predict(model2, alldata, allow.new.levels = T))
alldata$LMER_ho3 <- inv.logit(predict(model3, alldata, allow.new.levels = T))
alldata$LMER_ho4 <- inv.logit(predict(model4, alldata, allow.new.levels = T))
alldata$LMER_ho5 <- inv.logit(predict(model5, alldata, allow.new.levels = T))

alldata$LMER_OOS <- NA
alldata$LMER_OOS[alldata$fold_id == 1] <- alldata$LMER_ho1[alldata$fold_id == 1]
alldata$LMER_OOS[alldata$fold_id == 2] <- alldata$LMER_ho2[alldata$fold_id == 2]
alldata$LMER_OOS[alldata$fold_id == 3] <- alldata$LMER_ho3[alldata$fold_id == 3]
alldata$LMER_OOS[alldata$fold_id == 4] <- alldata$LMER_ho4[alldata$fold_id == 4]
alldata$LMER_OOS[alldata$fold_id == 5] <- alldata$LMER_ho5[alldata$fold_id == 5]



cor(alldata$val, alldata$LMER_OOS)^2
RMSE(alldata$val, alldata$LMER_OOS)
