rm(list = ls())
library(foreign)

model_date<- '2020_03_18_6'
outputdir <-  paste0('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/', model_date, '/')

# fitted_data <- read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/stackers/2020_03_17_3/fitted_stackers.csv', stringsAsFactors = F) 
custom_stage1_df <- read.csv(paste0(outputdir, '/custom_stage1_df.csv'), stringsAsFactors = F)
# fitted_child_models <- read.csv(paste0(outputdir, '/fitted_child_models.csv'), stringsAsFactors = F)
child_model_preds <- read.csv(paste0(outputdir, '/child_model_preds.csv'), stringsAsFactors = F)
fitted_stackers <- read.csv(paste0(outputdir, '/fitted_stackers.csv'), stringsAsFactors = F)

preds <- merge(child_model_preds, custom_stage1_df)
rm(child_model_preds, custom_stage1_df)

# plot(fitted_stackers$p, fitted_stackers$stacked_preds)
# cor(fitted_stackers$p, fitted_stackers$stacked_preds)^2
# preds[preds$location_id == 41 & preds$year_id == 2005,]

fitted_stackers <- fitted_stackers[c('location_id', 'year_id', 'p')]

mydata <- merge(preds, fitted_stackers, by = c('location_id', 'year_id'), all.x = T, all.y = T)

#get the location info
locs               <- read.dbf('/snfs1/DATA/SHAPE_FILES/GBD_geographies/master/GBD_2019/master/shapefiles/GBD2019_analysis_final_loc_set_22.dbf')
typhi_endemic      <- read.csv('/ihme/homes/annieb6/AMR/typhi_paratyphi/typhi_endemic_locations.csv', stringsAsFactors = F)
locs               <- locs[locs$loc_id%in% typhi_endemic$loc_id,]
locs               <- locs[locs$level == 3,]
locs               <- locs[c("loc_id", "loc_name", "spr_reg_id", "region_id", 'level', 'ihme_lc_id')]
mydata               <- merge(mydata, locs, by.x = 'location_id', by.y = 'loc_id', all.x = F, all.y = F)

pdf(paste0(outputdir, '/stackers.pdf'),
    height = 8.3, width = 11.7)

#plot out a page for each region
for(i in 1:length(unique(mydata$region_id))){
  subset <- mydata[mydata$region_id == unique(mydata$region_id)[i],]
  print(
    ggplot(subset)+
      geom_line(aes(x=year_id, y = cv_custom_stage_1), color = 'grey', size =2, alpha = .5)+
      geom_line(aes(x=year_id, y = gam),color = 'green')+
      geom_line(aes(x=year_id, y = rf), color = 'blue')+
      geom_line(aes(x=year_id, y = nnet), color = 'red')+
      geom_line(aes(x=year_id, y = ridge), color = 'purple')+
      # geom_pointrange(aes(x=year_id, y = val, ymin = lower_ci, ymax = upper_ci)) +
      geom_point(aes(x=year_id, y = p)) +
      scale_x_continuous("Year", 
                         breaks = seq(1990, 2018, 5),
                         labels = seq(1990, 2018, 5))+
      ylab('mdr')+
      theme_bw()+
      theme(legend.position = "bottom")+
      ggtitle(unique(subset$region_id))+      
      facet_wrap(~ihme_lc_id, nrow = ceiling(sqrt(length(unique(subset$location_id)))))+
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))+
      ylim(0,1)
  )
}
dev.off()
