# Calculate exceedance probabilities for STGPR models ####
rm(list = ls())
library(data.table)
library(sf)
library(ggplot2)
library(viridis)

# Combine the draws
MDR_Typhi_files <- list.files('/ihme/covariates/ubcov/model/output/174254/draws_temp_0')
FQNS_Typhi_files <- list.files('/ihme/covariates/ubcov/model/output/174260/draws_temp_0')
MDR_Paratyphi_files <- list.files('/ihme/covariates/ubcov/model/output/174614/draws_temp_0')
FQNS_Paratyphi_files <- list.files('/ihme/covariates/ubcov/model/output/176681/draws_temp_0')

for(f in MDR_Typhi_files){
  my_file <- fread(paste0('/ihme/covariates/ubcov/model/output/174254/draws_temp_0/', f))
  #bind together
  if(f == MDR_Typhi_files[1]){
    MDR_Typhi <- my_file
  }else{
    MDR_Typhi <-  rbind(MDR_Typhi, my_file)
  }
}

for(f in FQNS_Typhi_files){
  my_file <- fread(paste0('/ihme/covariates/ubcov/model/output/174260/draws_temp_0/', f))
  #bind together
  if(f == FQNS_Typhi_files[1]){
    FQNS_Typhi <- my_file
  }else{
    FQNS_Typhi <- rbind(FQNS_Typhi, my_file)
  }
}

for(f in MDR_Paratyphi_files){
  my_file <- fread(paste0('/ihme/covariates/ubcov/model/output/174614/draws_temp_0/', f))
  #bind together
  if(f == MDR_Paratyphi_files[1]){
    MDR_Paratyphi <- my_file
  }else{
    MDR_Paratyphi <-  rbind(MDR_Paratyphi, my_file)
  }
}

for(f in FQNS_Paratyphi_files){
  my_file <- fread(paste0('/ihme/covariates/ubcov/model/output/176681/draws_temp_0/', f))
  #bind together
  if(f == FQNS_Paratyphi_files[1]){
    FQNS_Paratyphi <- my_file
  }else{
    FQNS_Paratyphi <-  rbind(FQNS_Paratyphi, my_file)
  }
}

rm(my_file, f, MDR_Typhi_files, MDR_Paratyphi_files, FQNS_Paratyphi_files, FQNS_Typhi_files) 

# Limit to endemic locations
endemic_locs <- read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/typhi_endemic_locations2.csv', stringsAsFactors = F)
endemic_locs  <- endemic_locs[c("loc_id", "loc_name", "spr_reg_id", "region_id", 'level', 'ihme_lc_id')]
endemic_locs <- endemic_locs[endemic_locs$region_id != 65,]
endemic_locs <-  endemic_locs[endemic_locs$level == 3,]

MDR_Typhi <-  MDR_Typhi[MDR_Typhi$location_id %in% endemic_locs$loc_id,]
FQNS_Typhi <-  FQNS_Typhi[FQNS_Typhi$location_id %in% endemic_locs$loc_id,]

#and for paratyphi
endemic_locs <- endemic_locs[endemic_locs$spr_reg_id != 166,]
endemic_locs <- endemic_locs[endemic_locs$spr_reg_id != 137,]
MDR_Paratyphi <-  MDR_Paratyphi[MDR_Paratyphi$location_id %in% endemic_locs$loc_id,]
FQNS_Paratyphi <-  FQNS_Paratyphi[FQNS_Paratyphi$location_id %in% endemic_locs$loc_id,]

#calculate the probability of AMR exceeding 20%
MDR_Typhi[,5:1004][MDR_Typhi[,5:1004]<0.2] <- 0
MDR_Typhi[,5:1004][MDR_Typhi[,5:1004]>=0.2] <- 1
MDR_Typhi[,'exceedance_prob' := rowSums(MDR_Typhi[,5:1004])/10]
summary(MDR_Typhi$exceedance_prob)

FQNS_Typhi[,5:1004][FQNS_Typhi[,5:1004]<0.2] <- 0
FQNS_Typhi[,5:1004][FQNS_Typhi[,5:1004]>=0.2] <- 1
FQNS_Typhi[,'exceedance_prob' := rowSums(FQNS_Typhi[,5:1004])/10]
summary(FQNS_Typhi$exceedance_prob)

MDR_Paratyphi[,5:1004][MDR_Paratyphi[,5:1004]<0.2] <- 0
MDR_Paratyphi[,5:1004][MDR_Paratyphi[,5:1004]>=0.2] <- 1
MDR_Paratyphi[,'exceedance_prob' := rowSums(MDR_Paratyphi[,5:1004])/10]
summary(MDR_Paratyphi$exceedance_prob)

FQNS_Paratyphi[,5:1004][FQNS_Paratyphi[,5:1004]<0.2] <- 0
FQNS_Paratyphi[,5:1004][FQNS_Paratyphi[,5:1004]>=0.2] <- 1
FQNS_Paratyphi[,'exceedance_prob' := rowSums(FQNS_Paratyphi[,5:1004])/10]
summary(FQNS_Paratyphi$exceedance_prob)

#limit files
MDR_Typhi <- MDR_Typhi[,.(location_id, year_id, exceedance_prob, indicator = 'MDR_Typhi')] 
FQNS_Typhi <- FQNS_Typhi[,.(location_id, year_id, exceedance_prob, indicator = 'FQNS_Typhi')] 
MDR_Paratyphi <- MDR_Paratyphi[,.(location_id, year_id, exceedance_prob, indicator = 'MDR_Paratyphi')] 
FQNS_Paratyphi <- FQNS_Paratyphi[,.(location_id, year_id, exceedance_prob, indicator = 'FQNS_Paratyphi')] 

#read in the shapefile
IHME_shp <- st_read('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.shp')
IHME_shp <- IHME_shp[IHME_shp$level == 3,]
background <- st_read('/snfs1/WORK/11_geospatial/admin_shapefiles/00_raw_gaul/lbd_standard_admin_0.shp')

#merge to shapefile
MDR_Typhi <-  merge(IHME_shp, MDR_Typhi, by.x = "loc_id", by.y = 'location_id')
FQNS_Typhi <-  merge(IHME_shp, FQNS_Typhi, by.x = "loc_id", by.y = 'location_id')
MDR_Paratyphi <-  merge(IHME_shp, MDR_Paratyphi, by.x = "loc_id", by.y = 'location_id')
FQNS_Paratyphi <-  merge(IHME_shp, FQNS_Paratyphi, by.x = "loc_id", by.y = 'location_id')

# Plot out the figures you want
MDR_Typhi_1990 <- 
  ggplot()+
    geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
    geom_sf(data = MDR_Typhi[MDR_Typhi$year_id == 1990,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
    theme_bw()+
    theme(line = element_blank(),
          axis.text = element_blank())+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.position = "none")+
    scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
    labs(fill = NULL)+
    xlim(-20,150)+
    ylim(-35,55)

MDR_Typhi_2018 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = MDR_Typhi[MDR_Typhi$year_id == 2018,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(-20,150)+
  ylim(-35,55)

MDR_Paratyphi_1990 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = MDR_Paratyphi[MDR_Paratyphi$year_id == 1990,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(60, 150)+
  ylim(-12,55)

MDR_Paratyphi_2018 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = MDR_Paratyphi[MDR_Paratyphi$year_id == 2018,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(60, 150)+
  ylim(-12,55)

FQNS_Typhi_1990 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = FQNS_Typhi[FQNS_Typhi$year_id == 1990,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(-20,150)+
  ylim(-35,55)

FQNS_Typhi_2018 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = FQNS_Typhi[FQNS_Typhi$year_id == 2018,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(-20,150)+
  ylim(-35,55)

FQNS_Paratyphi_1990 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = FQNS_Paratyphi[FQNS_Paratyphi$year_id == 1990,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(60, 150)+
  ylim(-12,55)

FQNS_Paratyphi_2018 <- 
  ggplot()+
  geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0.15)+
  geom_sf(data = FQNS_Paratyphi[FQNS_Paratyphi$year_id == 2018,], aes(fill = exceedance_prob),colour = 'black', size = 0.15)+
  theme_bw()+
  theme(line = element_blank(),
        axis.text = element_blank())+
  theme(legend.position = "none")+
  scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
  labs(fill = NULL)+
  xlim(60, 150)+
  ylim(-12,55)

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/MDR_Typhi_1990.png',
    height = 10, width = 15, units = 'cm', res = 300)
  MDR_Typhi_1990
dev.off()
  
png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/MDR_Typhi_2018.png',
      height = 10, width = 15, units = 'cm', res = 300)
  MDR_Typhi_2018
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/FQNS_Typhi_1990.png',
    height = 10, width = 15, units = 'cm', res = 300)
FQNS_Typhi_1990
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/FQNS_Typhi_2018.png',
    height = 10, width = 15, units = 'cm', res = 300)
FQNS_Typhi_2018
dev.off()  

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/MDR_Paratyphi_1990.png',
    height = 10, width = 10, units = 'cm', res = 300)
MDR_Paratyphi_1990
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/MDR_Paratyphi_2018.png',
    height = 10, width = 10, units = 'cm', res = 300)
MDR_Paratyphi_2018
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/FQNS_Paratyphi_1990.png',
    height = 10, width = 10, units = 'cm', res = 300)
FQNS_Paratyphi_1990
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/exceedance_probabilities/FQNS_Paratyphi_2018.png',
    height = 10, width = 10, units = 'cm', res = 300)
FQNS_Paratyphi_2018
dev.off()  
