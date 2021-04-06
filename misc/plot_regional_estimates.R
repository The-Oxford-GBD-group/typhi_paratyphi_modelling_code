rm(list = ls())
library(data.table)
library(ggplot2)

#read in the super region percentges
for(ind in c('MDR_Typhi', 'FQNS_Typhi', 'MDR_Paratyphi', 'FQNS_Paratyphi')){
spr_reg_percentage <- fread(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/model_results/', ind, '_spr_reg_percentage.csv'))
spr_reg_percentage <- spr_reg_percentage[,.(region, year_id, mean, lower, upper)]

#and the national percentages
if(ind == 'MDR_Typhi'){run_id = 174254}
if(ind == 'FQNS_Typhi'){run_id = 174260}
if(ind == 'MDR_Paratyphi'){run_id = 174614}
if(ind == 'FQNS_Paratyphi'){run_id = 176681}

national_percentage <- fread(paste0('/ihme/covariates/ubcov/model/output/', run_id,'/model_estimates.csv'))
national_percentage <- national_percentage[national_percentage$level == 3,]
national_percentage$region[national_percentage$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
national_percentage$region[national_percentage$spr_reg_id == 158] <- 'South Asia'
national_percentage$region[national_percentage$spr_reg_id == 137] <- 'North Africa & Middle East'
national_percentage$region[national_percentage$spr_reg_id == 166] <- 'Sub-Saharan Africa'
national_percentage <- national_percentage[!is.na(national_percentage$region)]

png(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/model_results/', ind, '_spr_reg_percentage.png'), 
    height = 12, width = 20, units = 'cm', res = 300)
print(ggplot()+
  geom_boxplot(data = national_percentage, aes(group = year_id, x = year_id, y = gpr_mean*100))+
  geom_line(data = spr_reg_percentage, aes(x = year_id, y = mean), colour = 'red', size = 1)+
  geom_ribbon(data = spr_reg_percentage, aes(ymin = lower, ymax=upper, x = year_id), alpha = 0.25, fill = 'red') +
  ylim(0,100)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  labs(x = 'Year', y = paste0(unlist(strsplit(ind, '_'))[1], ' (%)'))+
  facet_wrap(~region)+
  theme(legend.position = "none"))
dev.off()

}
