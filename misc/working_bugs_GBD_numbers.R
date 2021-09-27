# Calculate the numbers of DR and FQNS by regions and globally
# weighted by the typhi/paratyphi incidence estimates
# propegating uncertainty through both models.
library(data.table)
library(ggplot2)
library(foreign)
rm(list = ls())

setwd('C:/Users/Annie/Documents/GRAM/typhi_paratyphi')
indicator = 'FQNS_Paratyphi'

#read in country (population weighted) draws
country_draws <- fread(paste0('model_results/bugs/admin1/', indicator, '/final/national_estimates.csv'))
country_draws$p.mean <- NULL
country_draws$p.lower <- NULL
country_draws$p.upper <- NULL

if(indicator == 'MDR_Typhi'| indicator == 'FQNS_Typhi'){
  GBD <- readRDS('GBD_estimates/Typhi_incidence_rate.rds')
} else{
  GBD <- readRDS('GBD_estimates/Paratyphi_incidence_rate.rds')
}

GBD$metric_id <-  NULL
GBD$cause_id <-  NULL
GBD$measure_id <-  NULL
GBD$age_group_id <- NULL
GBD$sex_id <- NULL
names(GBD) <- gsub('draw_', 'X', names(GBD))
names(GBD)[names(GBD)=='year_id'] <- 'year'

#get the country name from the loc_id
locs <- read.dbf("C:/Users/Annie/Documents/GRAM/shapefiles/GBD2019_analysis_final.dbf")
locs <- locs[locs$level == 3,]
locs$ihme_lc_id <- as.character(locs$ihme_lc_id)

GBD <- merge(GBD, locs[c('ihme_lc_id', 'loc_id')], by.x = 'location_id', by.y = 'loc_id')
GBD$location_id <- NULL
names(GBD)[names(GBD)=='ihme_lc_id'] <- 'COUNTRY_ID'

#Ensure the same countries and years are in the datasets and order the same
GBD2019 <- GBD[GBD$year == 2018,]
GBD2019$year <- 2019
GBD <- rbind(GBD, GBD2019)
rm(GBD2019)
GBD <-  GBD[GBD$COUNTRY_ID %in% country_draws$COUNTRY_ID,]

GBD <-  GBD[order(COUNTRY_ID, year)]
country_draws <-  country_draws[order(COUNTRY_ID, year)]

GBD <- data.frame(GBD)
names(GBD)[names(GBD) == 'X0'] <- 'X1000'
GBD <- GBD[names(country_draws)]

#convert the incidence rate to the number of cases
pop <- fread("C:/Users/Annie/Documents/GRAM/misc/GBD_total_populations.csv")
pop <- merge(pop, locs[c('ihme_lc_id', 'loc_id')], by.x = 'location_id', by.y = 'loc_id')

pop <- pop[pop$ihme_lc_id %in% GBD$COUNTRY_ID,]
pop2019 <- pop[pop$year_id == 2018,]
pop2019$year_id <- 2019
pop <- rbind(pop, pop2019)
pop <-  pop[order(ihme_lc_id, year_id)]
GBD[, 3:1002] <- GBD[, 3:1002] * pop$population 
GBD <- data.table(GBD)
rm(pop, pop2019)

# Get data frames for the sensitive isolates
DS_Typhi <- country_draws
DS_Typhi[, 3:1002] <- 1-DS_Typhi[, 3:1002]
DR_Typhi <- country_draws
rm(country_draws)

# Multiply the numbers together to get number of AMR cases
DR_Typhi[, 3:1002] <- DR_Typhi[, 3:1002]*GBD[, 3:1002]
DS_Typhi[, 3:1002] <- DS_Typhi[, 3:1002]*GBD[, 3:1002]

#merge on the regions
locs$region[locs$spr_reg_id == 137] <- 'North Africa & Middle East'
locs$region[locs$spr_reg_id == 158] <- 'South Asia'
locs$region[locs$spr_reg_id == 166] <- 'Sub-Saharan Africa'
locs$region[locs$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
locs$spr_reg_id <- NULL

GBD <-  merge(GBD, locs[c('ihme_lc_id', 'region')], by.x ='COUNTRY_ID', by.y = 'ihme_lc_id', all.x = T, all.y = F)
DR_Typhi <-  merge(DR_Typhi, locs[c('ihme_lc_id', 'region')], by.x ='COUNTRY_ID', by.y = 'ihme_lc_id', all.x = T, all.y = F)
DS_Typhi <-  merge(DS_Typhi, locs[c('ihme_lc_id', 'region')], by.x ='COUNTRY_ID', by.y = 'ihme_lc_id', all.x = T, all.y = F)

#collapse down to the global sums
DR_Typhi_global <- 
  DR_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year, .SDcols=3:1002] 

DS_Typhi_global <- 
  DS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year, .SDcols=3:1002] 

total_Typhi_global <- 
  GBD[, lapply(.SD, sum, na.rm=TRUE), by=year, .SDcols=3:1002] 

#collapse down to the super region sums
DR_Typhi_spr_reg <- 
  DR_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year'), .SDcols=3:1002] 

DS_Typhi_spr_reg <- 
  DS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year'), .SDcols=3:1002] 

total_Typhi_spr_reg <- 
  GBD[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year'), .SDcols=3:1002] 

#get the row means and uncertainty intervals
DR_Typhi_global$mean <- rowMeans(DR_Typhi_global[, 2:1001])
DR_Typhi_global$lower <- apply(DR_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
DR_Typhi_global$upper <- apply(DR_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

DS_Typhi_global$mean <- rowMeans(DS_Typhi_global[, 2:1001])
DS_Typhi_global$lower <- apply(DS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
DS_Typhi_global$upper <- apply(DS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

total_Typhi_global$mean <- rowMeans(total_Typhi_global[, 2:1001])
total_Typhi_global$lower <- apply(total_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
total_Typhi_global$upper <- apply(total_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

DR_Typhi_spr_reg$mean <- rowMeans(DR_Typhi_spr_reg[, 3:1002])
DR_Typhi_spr_reg$lower <- apply(DR_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
DR_Typhi_spr_reg$upper <- apply(DR_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

DS_Typhi_spr_reg$mean <- rowMeans(DS_Typhi_spr_reg[, 3:1002])
DS_Typhi_spr_reg$lower <- apply(DS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
DS_Typhi_spr_reg$upper <- apply(DS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

total_Typhi_spr_reg$mean <- rowMeans(total_Typhi_spr_reg[, 3:1002])
total_Typhi_spr_reg$lower <- apply(total_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
total_Typhi_spr_reg$upper <- apply(total_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

#Calculate the percentage of isolates with AMR globally
DR_Typhi_percentage_global  <- DR_Typhi_global[,1:1001]
DR_Typhi_percentage_global[, 2:1001] <- DR_Typhi_global[, 2:1001]/total_Typhi_global[, 2:1001]*100
DR_Typhi_percentage_global$mean <- rowMeans(DR_Typhi_percentage_global[, 2:1001])
DR_Typhi_percentage_global$lower <- apply(DR_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.025))
DR_Typhi_percentage_global$upper <- apply(DR_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.975))

#Calculate the percentage of isolates with AMR by super region
DR_Typhi_percentage_spr_reg  <- DR_Typhi_spr_reg[,1:1002]
DR_Typhi_percentage_spr_reg[, 3:1002] <- DR_Typhi_spr_reg[, 3:1002]/total_Typhi_spr_reg[, 3:1002]*100
DR_Typhi_percentage_spr_reg$mean <- rowMeans(DR_Typhi_percentage_spr_reg[, 3:1002])
DR_Typhi_percentage_spr_reg$lower <- apply(DR_Typhi_percentage_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
DR_Typhi_percentage_spr_reg$upper <- apply(DR_Typhi_percentage_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

#Restrict datasets to the mean and UIs for ease
DR_Typhi_global <- DR_Typhi_global[,.(year, DR = mean, DR_lower = lower, DR_upper = upper)]
DS_Typhi_global <- DS_Typhi_global[,.(year, `Non-DR` = mean, DS_lower = lower, DS_upper = upper)]
total_Typhi_global <- total_Typhi_global[,.(year, total = mean, total_lower = lower, total_upper = upper)]

DR_Typhi_spr_reg <- DR_Typhi_spr_reg[,.(region, year, DR = mean, DR_lower = lower, DR_upper = upper)]
DS_Typhi_spr_reg <- DS_Typhi_spr_reg[,.(region, year, `Non-DR` = mean, DS_lower = lower, DS_upper = upper)]
total_Typhi_spr_reg <- total_Typhi_spr_reg[,.(region, year, total = mean, total_lower = lower, total_upper = upper)]

DR_Typhi_percentage_global <- DR_Typhi_percentage_global[,.(year, DR = mean, DR_lower = lower, DR_upper = upper)]
DR_Typhi_percentage_spr_reg <- DR_Typhi_percentage_spr_reg[,.(region, year, DR = mean, DR_lower = lower, DR_upper = upper)]

#Merge datasets together
DR_Typhi_global <- merge(DR_Typhi_global, DS_Typhi_global) 
DR_Typhi_global <- merge(DR_Typhi_global, total_Typhi_global) 

DR_Typhi_spr_reg <- merge(DR_Typhi_spr_reg, DS_Typhi_spr_reg) 
DR_Typhi_spr_reg <- merge(DR_Typhi_spr_reg, total_Typhi_spr_reg) 

rm(DS_Typhi, DS_Typhi_global, DS_Typhi_spr_reg)

#plot out numbers
plot_DR_Typhi_global <- melt(DR_Typhi_global, id.vars = 'year', 
                              measure.vars = c('DR', 'Non-DR'),
                              value.name = 'number', variable.name = 'resistance')

plot_DR_Typhi_spr_reg <- melt(DR_Typhi_spr_reg, id.vars = c('year', 'region'), 
                               measure.vars = c('DR', 'Non-DR'),
                               value.name = 'number', variable.name = 'resistance')

#DR Typhi
png(paste0('model_results/GBD_numbers/', indicator, '_global_numbers.png'),
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_DR_Typhi_global, aes(x=year, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  theme(legend.position = "bottom")
dev.off()

png(paste0('model_results/GBD_numbers/', indicator, '_regional_numbers.png'),
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_DR_Typhi_spr_reg, aes(x=year, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#save estimates
write.csv(DR_Typhi_percentage_global, paste0('model_results/GBD_numbers/', indicator, '_global_percent.csv'), row.names = F)
write.csv(DR_Typhi_percentage_spr_reg, paste0('model_results/GBD_numbers/', indicator, '_spr_reg_percent.csv'), row.names = F)
write.csv(DR_Typhi_global, paste0('model_results/GBD_numbers/', indicator, '_global_numbers.csv'), row.names = F)
write.csv(DR_Typhi_spr_reg, paste0('model_results/GBD_numbers/', indicator, '_spr_reg_numbers.csv'), row.names = F)
