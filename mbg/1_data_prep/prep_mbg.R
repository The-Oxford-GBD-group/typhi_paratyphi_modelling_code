rm(list = ls())
library(data.table)

indicator = 'MDR_typhi'

master.data <- readRDS(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/', indicator, '.rds'))
master.data <-  data.table(master.data)

#limit to reqired variables
mydata <- master.data[,.(row_id, nid = source_id, super_region, region, country = iso3, 
                    adm1, adm2, best_geo, year = mid_year, n_resisant =(percentage_resistant/100)*sample_size,  N = sample_size, rate = percentage_resistant/100)]
mydata$n_resisant <- round(mydata$n_resisant,0)
colnames(mydata)[colnames(mydata) == 'n_resisant'] <- tolower(indicator)

mydata <- mydata[mydata$nid !=3286,] #Excluding this as the numbers look very odd and is in chinese so cannot double check atm
#remove those with <10 isolates * trying with 5
mydata <- mydata[mydata$N>=5,]

if(indicator == 'MDR_paratyphi' | indicator == 'FQNS_paratyphi'){
mydata <- mydata[mydata$region == 'South Asia' |
                   mydata$region == 'Southeast Asia' |
                   mydata$region == 'East Asia' |
                   mydata$region == 'South Asia',]
}

#remove those identified as outliers buy the GLM+1.5*MAD
outliers <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/identifying_outliers/', indicator, '/outlier_nids.csv'), stringsAsFactors = F)
outliers$outlier <- 1
mydata <- merge(mydata, outliers, by = c('nid', 'year'), all.x = T)
mydata <- mydata[is.na(mydata$outlier)]
mydata$outlier <- NULL

#select lowest level admin area
mydata$location_code <- mydata$adm2
mydata$location_code[is.na(mydata$location_code)] <- mydata$adm1[is.na(mydata$location_code)]
mydata$location_code <-  as.numeric(mydata$location_code)

#limit tp subnational data
mydata <- mydata[!is.na(mydata$location_code)]

#define which shapefile the codes are from 
mydata$shapefile <- 'admin2013_2'
mydata$shapefile[mydata$best_geo == 'admin1'] <- 'admin2013_1'
mydata$adm1 <-  NULL
mydata$adm2 <-  NULL
mydata$best_geo <-  NULL
mydata <- data.frame(mydata)

#resample polygons
dir.create(paste0('/share/homes/annieb6/AMR/typhi_paratyphi/resample_polys/', indicator), recursive = T, showWarnings = F)

locs_list <- unique(mydata[c('shapefile', 'location_code')])
locs_list <- locs_list[order(locs_list$shapefile),]
saveRDS(locs_list, paste0('/share/homes/annieb6/AMR/typhi_paratyphi/resample_polys/', indicator, '/locs_list.rds'))

shp_list <- unique(mydata$shapefile)

# qsub for each shapefile
# the resample polygons script

# for(s in shp_list){
  script             <- '/share/code/geospatial/annieb6/lbd_amr/typhi_paratyphi_modelling_code/mbg/1_data_prep/01b_resample_polys_loop.R'
  shell              <- '/share/code/geospatial/annieb6/lbd_core//mbg_central/share_scripts/shell_sing.sh'
  job_name           <- 'resample_polys'
  cores              <- 2
  runtime            <- '-l h_rt=00:01:00:00'
  memory             <- '-l m_mem_free=50G'
  proj               <- 'proj_geo_nodes'
  queue              <- 'geospatial.q'
  output             <- '/share/homes/annieb6/logs/output/'
  errors             <- '/share/homes/annieb6/logs/errors/'
  
  qsub <- paste('qsub',
                '-e', errors,
                '-o', output,
                '-P', proj,
                '-N', job_name,
                '-q', queue,
                '-cwd -l archive=TRUE',
                memory,
                '-l fthread=2',
                runtime,
                '-v sing_image=default',
                '-p 0',
                shell,
                script,
                'fin', sep =" ")
  system(qsub)
# }

resamples_polys <- list.files(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/resample_polys/', indicator, '/'), '.rds')
resamples_polys <- resamples_polys[c(-1)]

for(r in resamples_polys){
  my_poly <- readRDS(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/resample_polys/', indicator, '/', r))
  
  if(r==resamples_polys[1]){
    combined_polys <- my_poly
  } else{
    combined_polys <- rbind(combined_polys, my_poly)
  }
}

#merge points onto the data frane
mydata <- merge(mydata, combined_polys, by = c('shapefile', 'location_code'), all.x = T, all.y = T)
mydata$shapefile <-  NULL
mydata$location_code <-  NULL

#save data 
write.csv(mydata, paste0('/ihme/geospatial/mbg/input_data/', indicator, '2021_01_21.csv'), row.names = F)

          