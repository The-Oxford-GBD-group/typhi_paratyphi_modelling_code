#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Calculate and plot out the numbers of MDR  #
# and FQNS Typhi and Paratyphi based on GBD  # 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

library(data.table)
library(ggplot2)

rm(list = ls())

setwd('Z:/AMR/Pathogens/typhi_paratyphi')

#read in model estimates
mdr_typhi <- fread('model_results/STGPR/MDR_Typhi/model_estimates.csv')
fqns_typhi <- fread('model_results/STGPR/FQNS_Typhi/model_estimates.csv')
mdr_paratyphi <- fread('model_results/STGPR/MDR_Paratyphi/model_estimates.csv')
fqns_paratyphi <- fread('model_results/STGPR/FQNS_Paratyphi/model_estimates.csv')

#read in GBD estimates
GBD <- fread('GBD_estimates/GBD_Typhi_Paratyphi_estimates.csv')
GBD <- GBD[,.(location_id, cause_name, metric_name, year_id = year, val, upper, lower)]
GBD_typhi <- GBD[GBD$cause_name == 'Typhoid fever',]
GBD_paratyphi <- GBD[GBD$cause_name == 'Paratyphoid fever',]
GBD_typhi <- GBD_typhi[GBD_typhi$location_id %in% mdr_typhi$location_id,]
GBD_paratyphi <- GBD_paratyphi[GBD_paratyphi$location_id %in% mdr_paratyphi$location_id,]
rm(GBD)

#merge together
mdr_typhi <- merge(GBD_typhi, mdr_typhi, by = c('location_id', 'year_id'), all.x = F, all.y = T)
mdr_typhi$mdr <- mdr_typhi$gpr_mean*mdr_typhi$val
mdr_typhi$mdr_upper <- mdr_typhi$gpr_upper*mdr_typhi$upper
mdr_typhi$mdr_lower <- mdr_typhi$gpr_lower*mdr_typhi$lower
mdr_typhi$mds <- (1-mdr_typhi$gpr_mean)*mdr_typhi$val
mdr_typhi$mds_upper <- (1-mdr_typhi$gpr_upper)*mdr_typhi$upper
mdr_typhi$mds_lower <- (1-mdr_typhi$gpr_lower)*mdr_typhi$lower
mdr_typhi <- mdr_typhi[,.(spr_reg_id, region_id, location_id, loc_name, year_id, metric_name, mdr, mdr_upper, mdr_lower, mds, mds_upper, mds_lower)]

fqns_typhi <- merge(GBD_typhi, fqns_typhi, by = c('location_id', 'year_id'), all.x = F, all.y = T)
fqns_typhi$fqns <- fqns_typhi$gpr_mean*fqns_typhi$val
fqns_typhi$fqns_upper <- fqns_typhi$gpr_upper*fqns_typhi$upper
fqns_typhi$fqns_lower <- fqns_typhi$gpr_lower*fqns_typhi$lower
fqns_typhi$fqs <- (1-fqns_typhi$gpr_mean)*fqns_typhi$val
fqns_typhi$fqs_upper <- (1-fqns_typhi$gpr_upper)*fqns_typhi$upper
fqns_typhi$fqs_lower <- (1-fqns_typhi$gpr_lower)*fqns_typhi$lower
fqns_typhi <- fqns_typhi[,.(spr_reg_id, region_id, location_id, loc_name, year_id, metric_name, fqns, fqns_upper, fqns_lower, fqs, fqs_upper, fqs_lower)]

mdr_paratyphi <- merge(GBD_paratyphi, mdr_paratyphi, by = c('location_id', 'year_id'), all.x = F, all.y = T)
mdr_paratyphi$mdr <- mdr_paratyphi$gpr_mean*mdr_paratyphi$val
mdr_paratyphi$mdr_upper <- mdr_paratyphi$gpr_upper*mdr_paratyphi$upper
mdr_paratyphi$mdr_lower <- mdr_paratyphi$gpr_lower*mdr_paratyphi$lower
mdr_paratyphi$mds <- (1-mdr_paratyphi$gpr_mean)*mdr_paratyphi$val
mdr_paratyphi$mds_upper <- (1-mdr_paratyphi$gpr_upper)*mdr_paratyphi$upper
mdr_paratyphi$mds_lower <- (1-mdr_paratyphi$gpr_lower)*mdr_paratyphi$lower
mdr_paratyphi <- mdr_paratyphi[,.(spr_reg_id, region_id, location_id, loc_name, year_id, metric_name, mdr, mdr_upper, mdr_lower, mds, mds_upper, mds_lower)]

fqns_paratyphi <- merge(GBD_paratyphi, fqns_paratyphi, by = c('location_id', 'year_id'), all.x = F, all.y = T)
fqns_paratyphi$fqns <- fqns_paratyphi$gpr_mean*fqns_paratyphi$val
fqns_paratyphi$fqns_upper <- fqns_paratyphi$gpr_upper*fqns_paratyphi$upper
fqns_paratyphi$fqns_lower <- fqns_paratyphi$gpr_lower*fqns_paratyphi$lower
fqns_paratyphi$fqs <- (1-fqns_paratyphi$gpr_mean)*fqns_paratyphi$val
fqns_paratyphi$fqs_upper <- (1-fqns_paratyphi$gpr_upper)*fqns_paratyphi$upper
fqns_paratyphi$fqs_lower <- (1-fqns_paratyphi$gpr_lower)*fqns_paratyphi$lower
fqns_paratyphi <- fqns_paratyphi[,.(spr_reg_id, region_id, location_id, loc_name, year_id, metric_name, fqns, fqns_upper, fqns_lower, fqs, fqs_upper, fqs_lower)]

rm(GBD_typhi, GBD_paratyphi)

#get region names
mdr_typhi$region[mdr_typhi$spr_reg_id == 137] <- 'North Africa & Middle East'
mdr_typhi$region[mdr_typhi$spr_reg_id == 158] <- 'South Asia'
mdr_typhi$region[mdr_typhi$spr_reg_id == 166] <- 'Sub-Saharan Africa'
mdr_typhi$region[mdr_typhi$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'

fqns_typhi$region[fqns_typhi$spr_reg_id == 137] <- 'North Africa & Middle East'
fqns_typhi$region[fqns_typhi$spr_reg_id == 158] <- 'South Asia'
fqns_typhi$region[fqns_typhi$spr_reg_id == 166] <- 'Sub-Saharan Africa'
fqns_typhi$region[fqns_typhi$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'

mdr_paratyphi$region[mdr_paratyphi$spr_reg_id == 137] <- 'North Africa & Middle East'
mdr_paratyphi$region[mdr_paratyphi$spr_reg_id == 158] <- 'South Asia'
mdr_paratyphi$region[mdr_paratyphi$spr_reg_id == 166] <- 'Sub-Saharan Africa'
mdr_paratyphi$region[mdr_paratyphi$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'

fqns_paratyphi$region[fqns_paratyphi$spr_reg_id == 137] <- 'North Africa & Middle East'
fqns_paratyphi$region[fqns_paratyphi$spr_reg_id == 158] <- 'South Asia'
fqns_paratyphi$region[fqns_paratyphi$spr_reg_id == 166] <- 'Sub-Saharan Africa'
fqns_paratyphi$region[fqns_paratyphi$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'

# Caclualte the total numbers of resistant and sensitive infections by year and super-region/year ####
mdr_typhi_global <- mdr_typhi[metric_name == 'Number',.(MDR = sum(mdr),
                                    `Non-MDR` = sum(mds),
                                    all_typhi = (sum(mdr) + sum(mds))),
                                 by = 'year_id']

mdr_typhi_spr_reg <- mdr_typhi[metric_name == 'Number',.(MDR = sum(mdr),
                                    `Non-MDR` = sum(mds),
                                     all_typhi = (sum(mdr) + sum(mds))),
                                  by = c('region', 'year_id')]

fqns_typhi_global <- fqns_typhi[metric_name == 'Number',.(FQNS = sum(fqns),
                                                          FQS = sum(fqs)),
                                by = 'year_id']

fqns_typhi_spr_reg <- fqns_typhi[metric_name == 'Number',.(FQNS = sum(fqns),
                                                           FQS = sum(fqs)),
                                 by = c('region', 'year_id')]


mdr_paratyphi_global <- mdr_paratyphi[metric_name == 'Number',.(MDR = sum(mdr),
                                                                `Non-MDR` = sum(mds),
                                                                all_paratyphi = (sum(mdr) + sum(mds))),
                                      by = 'year_id']

mdr_paratyphi_spr_reg <- mdr_paratyphi[metric_name == 'Number',.(MDR = sum(mdr),
                                                                 `Non-MDR` = sum(mds),
                                                                 all_paratyphi = (sum(mdr) + sum(mds))),
                                       by = c('region', 'year_id')]

fqns_paratyphi_global <- fqns_paratyphi[metric_name == 'Number',.(FQNS = sum(fqns),
                                                                  FQS = sum(fqs)),
                                        by = 'year_id']

fqns_paratyphi_spr_reg <- fqns_paratyphi[metric_name == 'Number',.(FQNS = sum(fqns),
                                                                   FQS = sum(fqs)),
                                         by = c('region', 'year_id')]


#Reshape long for plotting ####
mdr_typhi_global <- melt(mdr_typhi_global, id.vars = 'year_id', 
                         measure.vars = c('MDR', 'Non-MDR'),
                         value.name = 'number', variable.name = 'resistance')

mdr_typhi_spr_reg <- melt(mdr_typhi_spr_reg, id.vars = c('year_id', 'region'), 
                          measure.vars = c('MDR', 'Non-MDR'),
                          value.name = 'number', variable.name = 'resistance')


fqns_typhi_global <- melt(fqns_typhi_global, id.vars = 'year_id', 
                          measure.vars = c('FQNS', 'FQS'),
                          value.name = 'number', variable.name = 'resistance')

fqns_typhi_spr_reg <- melt(fqns_typhi_spr_reg, id.vars = c('year_id', 'region'), 
                           measure.vars = c('FQNS', 'FQS'),
                           value.name = 'number', variable.name = 'resistance')


mdr_paratyphi_global <- melt(mdr_paratyphi_global, id.vars = 'year_id', 
                             measure.vars = c('MDR', 'Non-MDR'),
                             value.name = 'number', variable.name = 'resistance')

mdr_paratyphi_spr_reg <- melt(mdr_paratyphi_spr_reg, id.vars = c('year_id', 'region'), 
                              measure.vars = c('MDR', 'Non-MDR'),
                              value.name = 'number', variable.name = 'resistance')


fqns_paratyphi_global <- melt(fqns_paratyphi_global, id.vars = 'year_id', 
                              measure.vars = c('FQNS', 'FQS'),
                              value.name = 'number', variable.name = 'resistance')


fqns_paratyphi_spr_reg <- melt(fqns_paratyphi_spr_reg, id.vars = c('year_id', 'region'), 
                               measure.vars = c('FQNS', 'FQS'),
                               value.name = 'number', variable.name = 'resistance')


# plot out the numbers of drug resistant and drug susceptible cases ####
#MDR Typhi
png('model_results/figures/MDR_Typhi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(mdr_typhi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  theme(legend.position = "bottom")
dev.off()

png('model_results/figures/MDR_Typhi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(mdr_typhi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#FQ Typhi
png('model_results/figures/FQNS_Typhi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(fqns_typhi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  theme(legend.position = "bottom")
dev.off()

png('model_results/figures/FQNS_Typhi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(fqns_typhi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#MDR Paratyphi
png('model_results/figures/MDR_paratyphi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(mdr_paratyphi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  theme(legend.position = "bottom")
dev.off()

png('model_results/figures/MDR_paratyphi_regional_numbers.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(mdr_paratyphi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#FQ Paratyphi
png('model_results/figures/FQNS_paratyphi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(fqns_paratyphi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  theme(legend.position = "bottom")
dev.off()


png('model_results/figures/FQNS_paratyphi_regional_numbers.png',
    height = 10, width = 20, units = 'cm', res = 300)
ggplot(fqns_paratyphi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()
