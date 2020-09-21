rm(list = ls())
indicator = 'typhi_paratyphi'
#resample the admin 1 data

for (i in 1:2){
  mydata <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/mdr_typhi_mbg_adm', i,'.csv'), stringsAsFactors = F)
  
  locs_list <- unique(mydata[c('shapefile', 'location_code')])
  locs_list <- locs_list[order(locs_list$shapefile),]
  dir.create('/share/homes/annieb6/AMR/typhi_paratyphi/resample_polys', showWarnings = F)
  saveRDS(locs_list, '/share/homes/annieb6/AMR/typhi_paratyphi/resample_polys/locs_list.rds')

  script             <- '/share/code/geospatial/annieb6/lbd_amr/antibiotics/1_data_prep/resample_polys_loop.R'
  shell              <- '/share/code/geospatial/annieb6/lbd_core//mbg_central/share_scripts/shell_sing.sh'
  job_name           <- paste('admin2013', i,  'resample', sep = "_")
  cores              <- 5
  runtime            <- '-l h_rt=00:02:00:00'
  memory             <- '-l m_mem_free=10G'
  proj               <- 'proj_geospatial'
  queue              <- 'all.q'
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
                paste0('admin2013_', i),
                indicator,
                'fin', sep =" ")
  system(qsub)
}

#wait for resampling to finish
for(i in 1:2){ 
  mydata <- read.csv(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/datasets/mdr_typhi_mbg_adm', i,'.csv'), stringsAsFactors = F)
  polys <- readRDS(paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/resample_polys/resamples_admin2013_', i, '.rds'))
  mydata$weight <- NULL
  mydata <- merge(mydata, polys, by = c('location_code', 'shapefile'), all.x = T, all.y = T)
  mydata$location_code <-  NULL
  mydata$shapefile <- NULL
  mydata$cluster_id <- 1:nrow(mydata$latitude)
  write.csv(mydata, paste0('/share/geospatial/mbg/input_data/mdr_typhi_admin', i, '.csv'), row.names = F)
} 
