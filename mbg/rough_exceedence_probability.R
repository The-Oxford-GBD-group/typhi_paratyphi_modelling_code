rm(list = ls())
library(ggplot2)
library(viridis)

outdir = '/ihme/geospatial/mbg/lbd_amr/mdr_typhi/output/2020_07_26_04_10_02/model_validation/exceedance_probabilities'
years = seq(1990, 2015, 5)
admin_level = c(0, 1, 2)

#collapse to different admin levels
load('/ihme/geospatial/mbg/lbd_amr/mdr_typhi/output/2020_07_26_04_10_02/mdr_typhi_unraked_admin_draws_eb_bin0_0.RData')

#plot out the maps (just do 2018 first)
background <- st_read('/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_0.shp')
background <- st_simplify(background, dTolerance = 0.1, preserveTopology = T)

#for each admin level you want to do this for
for(i in admin_level){
  mydata <- get(paste0('admin_', i))
  mydata[,3:103][mydata[,3:103]<0.2] <- 0
  mydata[,3:103][mydata[,3:103]>=0.2] <- 1
  mydata[,'exceedance_prob' := rowSums(mydata[,3:103])]
  summary(mydata$exceedance_prob)
  mydata$exceedance_prob[mydata$exceedance_prob>100] <- 100

  shp <- st_read(paste0('/snfs1/WORK/11_geospatial/admin_shapefiles/current/lbd_standard_admin_', i, '.shp'))
  shp <- st_simplify(shp, dTolerance = 0.1, preserveTopology = T)
  
  plot_data <- merge(shp, mydata, all.x = F, all.y = T)
  plot_data <- plot_data[plot_data$year %in% years,]

  ## Plot 5 year estimates 
  png(paste0(outdir, '/admin_', i, '_TS_map.png'),
    height = 30, width = 20, units = 'cm', res = 300)
    print(
      ggplot()+
        geom_sf(data = background, fill = '#bdbdbd',colour = 'black', size = 0)+
        geom_sf(data = plot_data, aes(fill = exceedance_prob),colour = 'black', size = 0)+
        geom_sf(data = background, fill = NA, colour = 'black', size = 0.15)+
        theme_bw()+
        theme(line = element_blank(),
              axis.text = element_blank())+
        theme(plot.title = element_text(hjust = 0.5))+
        scale_fill_viridis(option='inferno', discrete = F, direction = -1, limits = c(0, 100))+
        labs(fill = 'Probability of >=20% MDR (%)')+
        xlim(-20, 150)+
        ylim(-35, 55)+
        facet_wrap(~year, ncol = floor(sqrt(length(years)))))
    dev.off()
}
