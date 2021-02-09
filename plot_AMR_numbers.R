rm(list = ls())
library(data.table)
library(foreign)

#Read in the AMR model results 
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

# Read in the incidence rates and order the columns to be the same as AMR files
GBD_Typhi <- readRDS('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/Typhi_incidence_rate.rds')
GBD_Typhi$metric_id <-  NULL
GBD_Typhi$cause_id <-  NULL
GBD_Typhi$measure_id <-  NULL
GBD_Typhi <-  data.frame(GBD_Typhi)
GBD_Typhi <- GBD_Typhi[names(MDR_Typhi)]
GBD_Typhi <-  data.table(GBD_Typhi)
GBD_Typhi <-  GBD_Typhi[order(location_id, year_id)]

GBD_Paratyphi <- readRDS('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/Paratyphi_incidence_rate.rds')
GBD_Paratyphi$metric_id <-  NULL
GBD_Paratyphi$cause_id <-  NULL
GBD_Paratyphi$measure_id <-  NULL
GBD_Paratyphi <-  data.frame(GBD_Paratyphi)
GBD_Paratyphi <- GBD_Paratyphi[names(MDR_Paratyphi)]
GBD_Paratyphi <-  data.table(GBD_Paratyphi)
GBD_Paratyphi <-  GBD_Paratyphi[order(location_id, year_id)]

#convert the incidence rate to the number of cases
pop <- fread('/ihme/homes/annieb6/GBD_populations/GBD_total_populations.csv')
pop <- pop[pop$location_id %in% GBD_Typhi$location_id,]
pop <-  pop[order(location_id, year_id)]

GBD_Typhi[, 5:1004] <- GBD_Typhi[, 5:1004] * pop$population
GBD_Paratyphi[, 5:1004] <- GBD_Paratyphi[, 5:1004] * pop$population

# Restrict the AMR files to endemic locations and order all files the same as GBD
MDR_Typhi <- MDR_Typhi[MDR_Typhi$location_id %in% GBD_Typhi$location_id,]
FQNS_Typhi <- FQNS_Typhi[FQNS_Typhi$location_id %in% GBD_Typhi$location_id,]
MDR_Paratyphi <- MDR_Paratyphi[MDR_Paratyphi$location_id %in% GBD_Paratyphi$location_id,]
FQNS_Paratyphi <- FQNS_Paratyphi[FQNS_Paratyphi$location_id %in% GBD_Paratyphi$location_id,]

MDR_Typhi <-  MDR_Typhi[order(location_id, year_id)]
FQNS_Typhi <- FQNS_Typhi[order(location_id, year_id)]
MDR_Paratyphi <-  MDR_Paratyphi[order(location_id, year_id)]
FQNS_Paratyphi <- FQNS_Paratyphi[order(location_id, year_id)]

# Get data frames for the sensitive isolates
MDS_Typhi <- MDR_Typhi 
MDS_Typhi[, 5:1004] <- 1-MDS_Typhi[, 5:1004]

FQS_Typhi <- FQNS_Typhi 
FQS_Typhi[, 5:1004] <- 1-FQS_Typhi[, 5:1004]

MDS_Paratyphi <- MDR_Paratyphi 
MDS_Paratyphi[, 5:1004] <- 1-MDS_Paratyphi[, 5:1004]

FQS_Paratyphi <- FQNS_Paratyphi 
FQS_Paratyphi[, 5:1004] <- 1-FQS_Paratyphi[, 5:1004]

# Multiply the numbers together to get number of AMR cases
MDR_Typhi[, 5:1004] <- MDR_Typhi[, 5:1004]*GBD_Typhi[, 5:1004]
MDS_Typhi[, 5:1004] <- MDS_Typhi[, 5:1004]*GBD_Typhi[, 5:1004]

FQNS_Typhi[, 5:1004] <- FQNS_Typhi[, 5:1004]*GBD_Typhi[, 5:1004]
FQS_Typhi[, 5:1004] <- FQS_Typhi[, 5:1004]*GBD_Typhi[, 5:1004]

MDR_Paratyphi[, 5:1004] <- MDR_Paratyphi[, 5:1004]*GBD_Paratyphi[, 5:1004]
MDS_Paratyphi[, 5:1004] <- MDS_Paratyphi[, 5:1004]*GBD_Paratyphi[, 5:1004]

FQNS_Paratyphi[, 5:1004] <- FQNS_Paratyphi[, 5:1004]*GBD_Paratyphi[, 5:1004]
FQS_Paratyphi[, 5:1004] <- FQS_Paratyphi[, 5:1004]*GBD_Paratyphi[, 5:1004]

#merge on the regions
locs <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
locs <- locs[c('loc_id', 'spr_reg_id')]
locs$region[locs$spr_reg_id == 137] <- 'North Africa & Middle East'
locs$region[locs$spr_reg_id == 158] <- 'South Asia'
locs$region[locs$spr_reg_id == 166] <- 'Sub-Saharan Africa'
locs$region[locs$spr_reg_id == 4] <- 'Southeast Asia, East Asia & Oceania'
locs$spr_reg_id <- NULL

GBD_Typhi <-  merge(GBD_Typhi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
GBD_Paratyphi <-  merge(GBD_Paratyphi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)

MDR_Typhi <-  merge(MDR_Typhi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
MDS_Typhi <-  merge(MDS_Typhi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
FQNS_Typhi <- merge(FQNS_Typhi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
FQS_Typhi <-  merge(FQS_Typhi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)

MDR_Paratyphi <-  merge(MDR_Paratyphi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
MDS_Paratyphi <-  merge(MDS_Paratyphi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
FQNS_Paratyphi <- merge(FQNS_Paratyphi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)
FQS_Paratyphi <-  merge(FQS_Paratyphi, locs, by.x ='location_id', by.y = 'loc_id', all.x = T, all.y = F)

#collapse down to the global and super region sums
MDR_Typhi_global <- 
  MDR_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

MDR_Typhi_spr_reg <- 
  MDR_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

MDS_Typhi_global <- 
  MDS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

MDS_Typhi_spr_reg <- 
  MDS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

FQNS_Typhi_global <- 
  FQNS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

FQNS_Typhi_spr_reg <- 
  FQNS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

FQS_Typhi_global <- 
  FQS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

FQS_Typhi_spr_reg <- 
  FQS_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

MDR_Paratyphi_global <- 
  MDR_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

MDR_Paratyphi_spr_reg <- 
  MDR_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

MDS_Paratyphi_global <- 
  MDS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

MDS_Paratyphi_spr_reg <- 
  MDS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

FQNS_Paratyphi_global <- 
  FQNS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

FQNS_Paratyphi_spr_reg <- 
  FQNS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

FQS_Paratyphi_global <- 
  FQS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

FQS_Paratyphi_spr_reg <- 
  FQS_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

total_Typhi_global <- 
  GBD_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

total_Typhi_spr_reg <- 
  GBD_Typhi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

total_Paratyphi_global <- 
  GBD_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=year_id, .SDcols=5:1004] 

total_Paratyphi_spr_reg <- 
  GBD_Paratyphi[, lapply(.SD, sum, na.rm=TRUE), by=c('region', 'year_id'), .SDcols=5:1004] 

#get the row means and uncertainty intervals
MDR_Typhi_global$mean <- rowMeans(MDR_Typhi_global[, 2:1001])
MDR_Typhi_global$lower <- apply(MDR_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDR_Typhi_global$upper <- apply(MDR_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

MDS_Typhi_global$mean <- rowMeans(MDS_Typhi_global[, 2:1001])
MDS_Typhi_global$lower <- apply(MDS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDS_Typhi_global$upper <- apply(MDS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQNS_Typhi_global$mean <- rowMeans(FQNS_Typhi_global[, 2:1001])
FQNS_Typhi_global$lower <- apply(FQNS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQNS_Typhi_global$upper <- apply(FQNS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQS_Typhi_global$mean <- rowMeans(FQS_Typhi_global[, 2:1001])
FQS_Typhi_global$lower <- apply(FQS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQS_Typhi_global$upper <- apply(FQS_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

MDR_Paratyphi_global$mean <- rowMeans(MDR_Paratyphi_global[, 2:1001])
MDR_Paratyphi_global$lower <- apply(MDR_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDR_Paratyphi_global$upper <- apply(MDR_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

MDS_Paratyphi_global$mean <- rowMeans(MDS_Paratyphi_global[, 2:1001])
MDS_Paratyphi_global$lower <- apply(MDS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDS_Paratyphi_global$upper <- apply(MDS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQNS_Paratyphi_global$mean <- rowMeans(FQNS_Paratyphi_global[, 2:1001])
FQNS_Paratyphi_global$lower <- apply(FQNS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQNS_Paratyphi_global$upper <- apply(FQNS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQS_Paratyphi_global$mean <- rowMeans(FQS_Paratyphi_global[, 2:1001])
FQS_Paratyphi_global$lower <- apply(FQS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQS_Paratyphi_global$upper <- apply(FQS_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

total_Typhi_global$mean <- rowMeans(total_Typhi_global[, 2:1001])
total_Typhi_global$lower <- apply(total_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
total_Typhi_global$upper <- apply(total_Typhi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

total_Paratyphi_global$mean <- rowMeans(total_Paratyphi_global[, 2:1001])
total_Paratyphi_global$lower <- apply(total_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.025))
total_Paratyphi_global$upper <- apply(total_Paratyphi_global[, 2:1001], 1, function(x) quantile(x, 0.975))

MDR_Typhi_spr_reg$mean <- rowMeans(MDR_Typhi_spr_reg[, 3:1002])
MDR_Typhi_spr_reg$lower <- apply(MDR_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
MDR_Typhi_spr_reg$upper <- apply(MDR_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

MDS_Typhi_spr_reg$mean <- rowMeans(MDS_Typhi_spr_reg[, 3:1002])
MDS_Typhi_spr_reg$lower <- apply(MDS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
MDS_Typhi_spr_reg$upper <- apply(MDS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

FQNS_Typhi_spr_reg$mean <- rowMeans(FQNS_Typhi_spr_reg[, 3:1002])
FQNS_Typhi_spr_reg$lower <- apply(FQNS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
FQNS_Typhi_spr_reg$upper <- apply(FQNS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

FQS_Typhi_spr_reg$mean <- rowMeans(FQS_Typhi_spr_reg[, 3:1002])
FQS_Typhi_spr_reg$lower <- apply(FQS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
FQS_Typhi_spr_reg$upper <- apply(FQS_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

MDR_Paratyphi_spr_reg$mean <- rowMeans(MDR_Paratyphi_spr_reg[, 3:1002])
MDR_Paratyphi_spr_reg$lower <- apply(MDR_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
MDR_Paratyphi_spr_reg$upper <- apply(MDR_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

MDS_Paratyphi_spr_reg$mean <- rowMeans(MDS_Paratyphi_spr_reg[, 3:1002])
MDS_Paratyphi_spr_reg$lower <- apply(MDS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
MDS_Paratyphi_spr_reg$upper <- apply(MDS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

FQNS_Paratyphi_spr_reg$mean <- rowMeans(FQNS_Paratyphi_spr_reg[, 3:1002])
FQNS_Paratyphi_spr_reg$lower <- apply(FQNS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
FQNS_Paratyphi_spr_reg$upper <- apply(FQNS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

FQS_Paratyphi_spr_reg$mean <- rowMeans(FQS_Paratyphi_spr_reg[, 3:1002])
FQS_Paratyphi_spr_reg$lower <- apply(FQS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
FQS_Paratyphi_spr_reg$upper <- apply(FQS_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

total_Typhi_spr_reg$mean <- rowMeans(total_Typhi_spr_reg[, 3:1002])
total_Typhi_spr_reg$lower <- apply(total_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
total_Typhi_spr_reg$upper <- apply(total_Typhi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

total_Paratyphi_spr_reg$mean <- rowMeans(total_Paratyphi_spr_reg[, 3:1002])
total_Paratyphi_spr_reg$lower <- apply(total_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.025))
total_Paratyphi_spr_reg$upper <- apply(total_Paratyphi_spr_reg[, 3:1002], 1, function(x) quantile(x, 0.975))

#Calculate the percentage of isolates with AMR globally
MDR_Typhi_percentage_global  <- MDR_Typhi_global[,1:1001]
MDR_Typhi_percentage_global[, 2:1001] <- MDR_Typhi_global[, 2:1001]/total_Typhi_global[, 2:1001]*100
MDR_Typhi_percentage_global$mean <- rowMeans(MDR_Typhi_percentage_global[, 2:1001])
MDR_Typhi_percentage_global$lower <- apply(MDR_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDR_Typhi_percentage_global$upper <- apply(MDR_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQNS_Typhi_percentage_global  <- FQNS_Typhi_global[,1:1001]
FQNS_Typhi_percentage_global[, 2:1001] <- FQNS_Typhi_global[, 2:1001]/total_Typhi_global[, 2:1001]*100
FQNS_Typhi_percentage_global$mean <- rowMeans(FQNS_Typhi_percentage_global[, 2:1001])
FQNS_Typhi_percentage_global$lower <- apply(FQNS_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQNS_Typhi_percentage_global$upper <- apply(FQNS_Typhi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.975))

MDR_Paratyphi_percentage_global  <- MDR_Paratyphi_global[,1:1001]
MDR_Paratyphi_percentage_global[, 2:1001] <- MDR_Paratyphi_global[, 2:1001]/total_Paratyphi_global[, 2:1001]*100
MDR_Paratyphi_percentage_global$mean <- rowMeans(MDR_Paratyphi_percentage_global[, 2:1001])
MDR_Paratyphi_percentage_global$lower <- apply(MDR_Paratyphi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.025))
MDR_Paratyphi_percentage_global$upper <- apply(MDR_Paratyphi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.975))

FQNS_Paratyphi_percentage_global  <- FQNS_Paratyphi_global[,1:1001]
FQNS_Paratyphi_percentage_global[, 2:1001] <- FQNS_Paratyphi_global[, 2:1001]/total_Paratyphi_global[, 2:1001]*100
FQNS_Paratyphi_percentage_global$mean <- rowMeans(FQNS_Paratyphi_percentage_global[, 2:1001])
FQNS_Paratyphi_percentage_global$lower <- apply(FQNS_Paratyphi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.025))
FQNS_Paratyphi_percentage_global$upper <- apply(FQNS_Paratyphi_percentage_global[, 2:1001], 1, function(x) quantile(x, 0.975))

#Restrict datasets to the mean and UIs for ease
MDR_Typhi_global <- MDR_Typhi_global[,.(year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
MDS_Typhi_global <- MDS_Typhi_global[,.(year_id, `Non-MDR` = mean, MDS_lower = lower, MDS_upper = upper)]
FQNS_Typhi_global <- FQNS_Typhi_global[,.(year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]
FQS_Typhi_global <- FQS_Typhi_global[,.(year_id, `Non-FQNS` = mean, FQS_lower = lower, FQS_upper = upper)]
MDR_Paratyphi_global <- MDR_Paratyphi_global[,.(year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
MDS_Paratyphi_global <- MDS_Paratyphi_global[,.(year_id, `Non-MDR` = mean, MDS_lower = lower, MDS_upper = upper)]
FQNS_Paratyphi_global <- FQNS_Paratyphi_global[,.(year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]
FQS_Paratyphi_global <- FQS_Paratyphi_global[,.(year_id, `Non-FQNS` = mean, FQS_lower = lower, FQS_upper = upper)]
total_Typhi_global <- total_Typhi_global[,.(year_id, total = mean, total_lower = lower, total_upper = upper)]
total_Paratyphi_global <- total_Paratyphi_global[,.(year_id, total = mean, total_lower = lower, total_upper = upper)]

MDR_Typhi_percentage_global <- MDR_Typhi_percentage_global[,.(year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
FQNS_Typhi_percentage_global <- FQNS_Typhi_percentage_global[,.(year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]
MDR_Paratyphi_percentage_global <- MDR_Paratyphi_percentage_global[,.(year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
FQNS_Paratyphi_percentage_global <- FQNS_Paratyphi_percentage_global[,.(year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]

MDR_Typhi_spr_reg <- MDR_Typhi_spr_reg[,.(region, year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
MDS_Typhi_spr_reg <- MDS_Typhi_spr_reg[,.(region, year_id, `Non-MDR` = mean, MDS_lower = lower, MDS_upper = upper)]
FQNS_Typhi_spr_reg <- FQNS_Typhi_spr_reg[,.(region, year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]
FQS_Typhi_spr_reg <- FQS_Typhi_spr_reg[,.(region, year_id, `Non-FQNS` = mean, FQS_lower = lower, FQS_upper = upper)]
MDR_Paratyphi_spr_reg <- MDR_Paratyphi_spr_reg[,.(region, year_id, MDR = mean, MDR_lower = lower, MDR_upper = upper)]
MDS_Paratyphi_spr_reg <- MDS_Paratyphi_spr_reg[,.(region, year_id, `Non-MDR` = mean, MDS_lower = lower, MDS_upper = upper)]
FQNS_Paratyphi_spr_reg <- FQNS_Paratyphi_spr_reg[,.(region, year_id, FQNS = mean, FQNS_lower = lower, FQNS_upper = upper)]
FQS_Paratyphi_spr_reg <- FQS_Paratyphi_spr_reg[,.(region, year_id, `Non-FQNS` = mean, FQS_lower = lower, FQS_upper = upper)]
total_Typhi_spr_reg <- total_Typhi_spr_reg[,.(region, year_id, total = mean, total_lower = lower, total_upper = upper)]
total_Paratyphi_spr_reg <- total_Paratyphi_spr_reg[,.(region, year_id, total = mean, total_lower = lower, total_upper = upper)]

#Merge datasets together
MDR_Typhi_global <- merge(MDR_Typhi_global, MDS_Typhi_global) 
MDR_Typhi_global <- merge(MDR_Typhi_global, total_Typhi_global) 

MDR_Typhi_spr_reg <- merge(MDR_Typhi_spr_reg, MDS_Typhi_spr_reg) 
MDR_Typhi_spr_reg <- merge(MDR_Typhi_spr_reg, total_Typhi_spr_reg) 

FQNS_Typhi_global <- merge(FQNS_Typhi_global, FQS_Typhi_global) 
FQNS_Typhi_global <- merge(FQNS_Typhi_global, total_Typhi_global) 

FQNS_Typhi_spr_reg <- merge(FQNS_Typhi_spr_reg, FQS_Typhi_spr_reg) 
FQNS_Typhi_spr_reg <- merge(FQNS_Typhi_spr_reg, total_Typhi_spr_reg) 

MDR_Paratyphi_global <- merge(MDR_Paratyphi_global, MDS_Paratyphi_global) 
MDR_Paratyphi_global <- merge(MDR_Paratyphi_global, total_Paratyphi_global) 

MDR_Paratyphi_spr_reg <- merge(MDR_Paratyphi_spr_reg, MDS_Paratyphi_spr_reg) 
MDR_Paratyphi_spr_reg <- merge(MDR_Paratyphi_spr_reg, total_Paratyphi_spr_reg) 

FQNS_Paratyphi_global <- merge(FQNS_Paratyphi_global, FQS_Paratyphi_global) 
FQNS_Paratyphi_global <- merge(FQNS_Paratyphi_global, total_Paratyphi_global) 

FQNS_Paratyphi_spr_reg <- merge(FQNS_Paratyphi_spr_reg, FQS_Paratyphi_spr_reg) 
FQNS_Paratyphi_spr_reg <- merge(FQNS_Paratyphi_spr_reg, total_Paratyphi_spr_reg) 

rm(MDS_Paratyphi, MDS_Paratyphi_global, MDS_Paratyphi_spr_reg, MDS_Typhi, MDS_Typhi_global, MDS_Typhi_spr_reg,
   FQS_Paratyphi, FQS_Paratyphi_global, FQS_Paratyphi_spr_reg, FQS_Typhi, FQS_Typhi_global, FQS_Typhi_spr_reg)

#plot out numbers
plot_MDR_Typhi_global <- melt(MDR_Typhi_global, id.vars = 'year_id', 
                         measure.vars = c('MDR', 'Non-MDR'),
                         value.name = 'number', variable.name = 'resistance')

plot_MDR_Typhi_spr_reg <- melt(MDR_Typhi_spr_reg, id.vars = c('year_id', 'region'), 
                          measure.vars = c('MDR', 'Non-MDR'),
                          value.name = 'number', variable.name = 'resistance')

plot_FQNS_Typhi_global <- melt(FQNS_Typhi_global, id.vars = 'year_id', 
                              measure.vars = c('FQNS', 'Non-FQNS'),
                              value.name = 'number', variable.name = 'resistance')

plot_FQNS_Typhi_spr_reg <- melt(FQNS_Typhi_spr_reg, id.vars = c('year_id', 'region'), 
                               measure.vars = c('FQNS', 'Non-FQNS'),
                               value.name = 'number', variable.name = 'resistance')


plot_MDR_Paratyphi_global <- melt(MDR_Paratyphi_global, id.vars = 'year_id', 
                              measure.vars = c('MDR', 'Non-MDR'),
                              value.name = 'number', variable.name = 'resistance')

plot_MDR_Paratyphi_spr_reg <- melt(MDR_Paratyphi_spr_reg, id.vars = c('year_id', 'region'), 
                               measure.vars = c('MDR', 'Non-MDR'),
                               value.name = 'number', variable.name = 'resistance')

plot_FQNS_Paratyphi_global <- melt(FQNS_Paratyphi_global, id.vars = 'year_id', 
                               measure.vars = c('FQNS', 'Non-FQNS'),
                               value.name = 'number', variable.name = 'resistance')

plot_FQNS_Paratyphi_spr_reg <- melt(FQNS_Paratyphi_spr_reg, id.vars = c('year_id', 'region'), 
                                measure.vars = c('FQNS', 'Non-FQNS'),
                                value.name = 'number', variable.name = 'resistance')

#MDR Typhi
png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/MDR_Typhi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_MDR_Typhi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  theme(legend.position = "bottom")
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/MDR_Typhi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_MDR_Typhi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#FQNS Typhi
png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/FQNS_Typhi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_FQNS_Typhi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  theme(legend.position = "bottom")
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/FQNS_Typhi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_FQNS_Typhi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#MDR Paratyphi
png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/MDR_Paratyphi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_MDR_Paratyphi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  theme(legend.position = "bottom")
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/MDR_Paratyphi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_MDR_Paratyphi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#08589e', '#7bccc4'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

#FQNS Paratyphi
png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/FQNS_Paratyphi_global_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_FQNS_Paratyphi_global, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  theme(legend.position = "bottom")
dev.off()

png('/ihme/homes/annieb6/AMR/typhi_paratyphi/GBD_numbers/FQNS_Paratyphi_regional_numbers.png',
    height = 20, width = 20, units = 'cm', res = 300)
ggplot(plot_FQNS_Paratyphi_spr_reg, aes(x=year_id, y = number/100000, fill=resistance)) +
  geom_area()+
  theme_bw()+
  scale_x_continuous(name = 'Year', breaks = seq(1990, 2015, 5), expand = c(0, 0))+
  scale_y_continuous(name = 'Number of infections (millions)',  expand = c(0, 0))+
  scale_fill_manual(name = NULL, values = c('#810f7c', '#bfd3e6'))+
  facet_wrap(~region, scales = 'free_y')+
  theme(legend.position = "bottom")
dev.off()

