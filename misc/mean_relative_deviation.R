rm(list = ls())
library(data.table)

setwd('C:/Users/Annie/Documents/GRAM/typhi_paratyphi/model_results')
indicator = 'FQNS_Typhi'

admin1 <- fread(paste0('bugs/admin1/', indicator, '/final/bugs_final.csv'))
admin0 <- fread(paste0('bugs/admin1/', indicator, '/final/national_estimates.csv'))

admin1 <- admin1[,.(COUNTRY_ID, adj_id, year, p.mean)]
admin0 <- admin0[,.(COUNTRY_ID, year, country_mean = p.mean)]

#merge the admin 0 and admin 1
mydata <- merge(admin1, admin0, all.x = F, all.y = F)

#Calculate the deviation from the mean
mydata$absolute_deviation <- mydata$p.mean - mydata$country_mean
mydata$relative_deviation <- mydata$absolute_deviation/mydata$country_mean

#Get the values for the most deviation above and below the mean for each country, and the min and max admin 2 estimates
deviations <- mydata[,.(min_adm1 = min(p.mean),
                        max_adm1 = max(p.mean),
                        min_deviation = min(absolute_deviation),
                        max_deviation = max(absolute_deviation),
                        mean_deviation = mean(abs(relative_deviation))),
                     by = c('COUNTRY_ID', 'year', 'country_mean')]

write.csv(deviations, paste0('subnat_variation/', indicator, '.csv'), row.names = F)
