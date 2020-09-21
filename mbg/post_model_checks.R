#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Model data checks - working #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# i. Setup the packages, run information and functions required #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# run_date                 <- as.character(commandArgs()[6])
# indicator                <- as.character(commandArgs()[9])
# indicator_group          <- as.character(commandArgs()[10])
run_date                 <- "2020_05_21_07_16_27"
indicator                <- 'mdr_typhi'
indicator_group          <- 'lbd_amr'
core_repo                <- '/share/code/geospatial/annieb6/lbd_core/'

# Load MBG packages and functions
package_list      <- readLines(paste0(core_repo, "/mbg_central/share_scripts/common_inputs/package_list.csv"))
message('Loading in required R packages and MBG functions')
source(paste0(core_repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = core_repo)

# Load additional functions and libraries
libs <- c('RColorBrewer', 'sp', 'sf', 'ggpubr', 'ggrepel')
lapply(libs, library, character.only = TRUE, quietly = TRUE)
rm(libs)

source('/share/code/geospatial/annieb6/lbd_amr/diarrhea_antibiotics/4_post_processing/model_validation_functions.R')

# #Re-load config file
# config <- set_up_config(repo = core_repo,
#                         indicator = indicator,
#                         indicator_group = indicator_group,
#                         run_date = run_date,
#                         post_est_only = TRUE)
# 
# if (class(year_list) == "character") year_list <- eval(parse(text=year_list))
# if (class(Regions) == "character" & length(Regions) == 1) Regions <- eval(parse(text=Regions))
outputdir <- file.path('/share/geospatial/mbg',indicator_group,indicator,'output',run_date,'/model_validation/')
dir.create(outputdir, showWarnings = F)
# input_data_date <- datatag
Regions <- c('wssa+cssa')
modeling_shapefile_version = 'current'
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # 1. Plot out the time series maps and eye-ball results #
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# plot_all_regions(run_date = run_date,
#                  indicator = indicator,
#                  indicator_group = indicator_group,
#                  outputdir = outputdir,
#                  my.palette =  brewer.pal(n = 10, name = "RdYlGn"),
#                  summstats = 'mean',
#                  years = 1990:2018,
#                  title = 'MDR S Typhi (%)')
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # 2. Compare estimates to raw data & #
# #    compare to previous model runs  #
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# y_axis_name = 'mdr_typhi'
# outdir = paste(outputdir, '/time_series_plots')
# dir.create(outdir, recursive = T)
# 
# mydata <- read.csv(paste0('/ihme/geospatial/mbg/',
#                           indicator_group,
#                           '/',
#                           indicator,
#                           '/output/',
#                           run_date,
#                           '/pred_derivatives/admin_summaries/',
#                           indicator,
#                           '_admin_0_unraked_summary.csv'), 
#                    stringsAsFactors = F)
# 
# input_data <- read.csv('/share/homes/annieb6/AMR/typhi_paratyphi/datasets/MDR_typhi_outliered.csv', stringsAsFactors = F)
# 
# gaul_codes <- get_adm0_codes(unique(mydata$region), shapefile_version = '2020_02_20') 
# 
# gaul_to_loc_id <- 
#   get_location_code_mapping(shapefile_version = '2020_02_20') %>% 
#   dplyr::select(GAUL_CODE, loc_name, ihme_lc_id) %>% 
#   dplyr::rename(location_name = loc_name) %>%
#   filter(GAUL_CODE %in% gaul_codes)
# 
# input_data <- merge(input_data, gaul_to_loc_id, by.x = 'country', by.y = 'ihme_lc_id', all.x = T)
# input_data <-  input_data[input_data$location_name %in% mydata$ADM0_NAME,]
# input_data$region <- NULL
# 
# mydata <- merge(mydata, input_data, by.x = c('ADM0_NAME', 'year'), by.y = c('location_name', 'year_id'), all.x = T, all.y = T)
# 
# pdf(paste0(outputdir, '/National_time_series_plots.pdf'),
#     height = 8.3, width = 11.7)
# 
# for(i in 1:length(unique(mydata$region))){
#   subset <- mydata[mydata$region == unique(mydata$region)[i],]
#   print(
#     ggplot(subset)+
#       geom_line(aes(x=year, y = mean), color = 'purple')+
#       geom_ribbon(aes(ymin = lower, ymax=upper, x = year), alpha = 0.1, fill = 'purple') +
#       geom_point(aes(x=year, y = val), alpha = 0.5, fill = 'grey') +
#       scale_x_continuous("Year", 
#                          breaks = seq(1990, 2018, 5),
#                          labels = c('1990', '1995', "2000", "2005", "2010", "2015"))+
#       ylim(0,1)+
#       ylab(y_axis_name)+
#       theme_bw()+
#       facet_wrap(~ADM0_NAME, nrow = 3)+
#       ggtitle(unique(subset$region))+     
#       theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#       theme(plot.title = element_text(hjust = 0.5))
#   )
# }
# dev.off()
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # 3. Plot the parts of the model & covariates #
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 
# #b. Covariate importance plots
# get_cov_weights(indicator,
#                 indicator_group,
#                 run_date,
#                 regions = Regions,
#                 outdir =  paste0(outputdir, '/covariate_importance/'))
# 
# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # # 4. Analyse hyperparameters #
# # #~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# library(gridExtra)
# dir.create(paste0(outputdir, '/hyperparameters/'))
# 
# # Create table of hyperparameters
# use_stacking_covs = TRUE
# use_gp = TRUE
# hyperparameters <- clean_model_results_table()
# 
# # hyperparameters$cssa_0$cssa <- NA
# # hyperparameters$wssa_0$wssa <- NA
# # hyperparameters$`essa-yem_0`$essa <- NA
# # hyperparameters$sssa_0$sssa <- NA
# # hyperparameters$noaf_0$noaf <- NA
# 
# pdf(paste0(outputdir, '/hyperparameters/hyperparameter_tables.pdf'))
# lapply(hyperparameters, function(x){
# table <- tableGrob(x)
# grid.newpage()
# h <- grobHeight(table)
# w <- grobWidth(table)
# title <- textGrob(names(x)[5], y=unit(0.5,"npc") + h,
#                   vjust=0, gp=gpar(fontsize=20))
# gt <- gTree(children=gList(table, title))
# grid.draw(gt)})
# dev.off()
# 
# lapply(hyperparameters, function(x){
#   names(x)[6]
# })
# 
# # Plots of hyperparameters
# plot_hyperparameters(indicator = indicator,
#                      indicator_group = indicator_group,
#                      run_date = run_date,
#                      age = 0,
#                      holdout = 0,
#                      save_file = paste0(outputdir, '/hyperparameters/inla_hyperparameters.pdf'),
#                      regions = Regions)
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # Check colinearity in GAM #
# #~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 
# #a. calculate the pairwise correlation between all covariates
# covariate_corr_matrix (indicator = indicator,
#                         indicator_group = indicator_group,
#                         run_date = run_date,
#                         regions = Regions,
#                         out_dir = paste0(outputdir, '/covariate_correlation/'))
# 
# 
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# # Get out of sample fit stats #
# # for aggregated adm0, 1 & 2  #
# #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# run_in_oos <- get_is_oos_draws(ind_gp = indicator_group,
#                                ind = indicator,
#                                rd = run_date,
#                                ind_fm = 'binomial',
#                                age = 0,
#                                nperiod = 19,
#                                yrs = 2000:2018,
#                                get.oos = as.logical(makeholdouts),
#                                write.to.file = TRUE,
#                                year_col = 'year',
#                                shapefile_version = modeling_shapefile_version)
# 
# ## set out_dir
# out_dir <- paste0(outputdir, "/oos_metrics/")
# dir.create(out_dir, recursive = T, showWarnings = F)
# 
# ## for admin0
# draws.df <- fread(sprintf("/share/geospatial/mbg/%s/%s/output/%s/output_draws_data.csv",
#                           indicator_group, indicator, run_date))
# 
# unique(draws.df$region)
# #clean up region names
# draws.df$region[draws.df$region == "balkans_ext+caucasus"] <-  'Balkans & Caucasus'
# draws.df$region[draws.df$region == "dia_mcaca"] <-  'Central America & Caribbean'
# draws.df$region[draws.df$region == "dia_central_asia+kaz+mng"] <-  'Central Asia'
# draws.df$region[draws.df$region == "dia_cssa"] <-  'Central sub-Saharan Africa'
# draws.df$region[draws.df$region == "dia_essa+dia_sssa-syc"] <-  'Eastern & Southern sub-Saharan Africa'
# draws.df$region[draws.df$region == "dia_afr_horn"] <-  'Horn of Africa'
# draws.df$region[draws.df$region == "dia_malay+dia_oceania"] <-  'Malay & Oceania'
# draws.df$region[draws.df$region == "dia_mid_east"] <-  'Middle East'
# draws.df$region[draws.df$region == "dia_name"] <-  'North Africa'
# draws.df$region[draws.df$region == "dia_s_america-tto"] <-  'South America'
# draws.df$region[draws.df$region == "dia_south_asia"] <-  'South Asia'
# draws.df$region[draws.df$region == "dia_se_asia"] <-  'Southeast Asia'
# draws.df$region[draws.df$region == "dia_wssa"] <-  'Western sub-Saharan Africa'
# 
# country.pvtable <- get_pv_table(d = draws.df,
#                                 indicator = indicator,
#                                 indicator_group = indicator_group,
#                                 rd = run_date,
#                                 aggregate_on='country',
#                                 draws = as.numeric(samples),
#                                 coverage_probs = c(95),
#                                 result_agg_over =  c('oos', 'region'),
#                                 weighted = TRUE,
#                                 family = 'binomial',
#                                 plot = TRUE,
#                                 plot_by = 'region',
#                                 plot_by_title = 'region',
#                                 plot_ci = FALSE,
#                                 plot_ci_level = 95,
#                                 ci_color = "grey",
#                                 point_alpha = 1,
#                                 point_color = "black",
#                                 plot_title = indicator,
#                                 plot_ncol = 4,
#                                 save_csv = T,
#                                 out.dir = out_dir) 
# 
# country.pvtable <- get_pv_table(d = draws.df,
#                                 indicator = indicator,
#                                 indicator_group = indicator_group,
#                                 rd = run_date,
#                                 aggregate_on='country',
#                                 draws = as.numeric(samples),
#                                 coverage_probs = c(95),
#                                 result_agg_over =  c('oos'),
#                                 weighted = TRUE,
#                                 family = 'binomial',
#                                 plot = TRUE,
#                                 # plot_by = 'region',
#                                 # plot_by_title = 'region',
#                                 plot_ci = FALSE,
#                                 plot_ci_level = 95,
#                                 ci_color = "grey",
#                                 point_alpha = 1,
#                                 point_color = "black",
#                                 plot_title = indicator,
#                                 plot_ncol = 4,
#                                 save_csv = T,
#                                 out.dir = paste0(out_dir, '/country/')) 
# 
# ad1.pvtable <- get_pv_table(d = draws.df,
#                             indicator = indicator,
#                             indicator_group = indicator_group,
#                             rd = run_date,
#                             aggregate_on='ad1',
#                             draws = as.numeric(samples),
#                             coverage_probs = c(95),
#                             result_agg_over =  c('oos', 'region'),
#                             weighted = TRUE,
#                             family = 'binomial',
#                             plot = TRUE,
#                             plot_by = 'region',
#                             plot_by_title = 'region',
#                             plot_ci = FALSE,
#                             plot_ci_level = 95,
#                             ci_color = "grey",
#                             point_alpha = 1,
#                             point_color = "black",
#                             plot_title = indicator,
#                             plot_ncol = 4,
#                             save_csv = T,
#                             out.dir = paste0(out_dir, '/admin1/'))
# 
# ad1.pvtable <- get_pv_table(d = draws.df,
#                             indicator = indicator,
#                             indicator_group = indicator_group,
#                             rd = run_date,
#                             aggregate_on='ad1',
#                             draws = as.numeric(samples),
#                             coverage_probs = c(95),
#                             result_agg_over =  c('oos'),
#                             weighted = TRUE,
#                             family = 'binomial',
#                             plot = TRUE,
#                             # plot_by = 'region',
#                             # plot_by_title = 'region',
#                             plot_ci = FALSE,
#                             plot_ci_level = 95,
#                             ci_color = "grey",
#                             point_alpha = 1,
#                             point_color = "black",
#                             plot_title = indicator,
#                             plot_ncol = 4,
#                             save_csv = T,
#                             out.dir = paste0(out_dir, '/admin1/'))
# 
# ad2.pvtable <- get_pv_table(d = draws.df,
#                             indicator = indicator,
#                             indicator_group = indicator_group,
#                             rd = run_date,
#                             aggregate_on='ad2',
#                             draws = as.numeric(samples),
#                             coverage_probs = c(95),
#                             result_agg_over =  c('year','oos', 'region'),
#                             weighted = TRUE,
#                             family = 'binomial',
#                             plot = TRUE,
#                             plot_by = 'region',
#                             plot_by_title = 'region',
#                             plot_ci = FALSE,
#                             plot_ci_level = 95,
#                             ci_color = "grey",
#                             point_alpha = 1,
#                             point_color = "black",
#                             plot_title = indicator,
#                             plot_ncol = 4,
#                             save_csv = T,
#                             out.dir = out_dir)
# 
#~~~~~~~~~~~~~~~~~~~~~~~~~#
# Plot the child stackers #
#~~~~~~~~~~~~~~~~~~~~~~~~~#
plot_child_stackers(indicator = indicator,
                    indicator_group = indicator_group,
                    run_date = run_date,
                    regions = Regions,
                    start_year = 1990,
                    end_year = 2018,
                    out_dir = paste0(outputdir, '/stackers'),
                    pop_measure = 'a0004t')

# test_rho_priors("list(prior = 'normal', param = c(2, 0.5))")

#~~~~~~~~~~~~~~~~~~~~~~#
# Plot absolute errors #
#~~~~~~~~~~~~~~~~~~~~~~#
# dir.create(paste0(outputdir, '/abs_error/'))
# gaul_list <- get_adm0_codes(regions,
#                             shapefile_version = modeling_shapefile_version)
#
# abs_err_plot_list <- plot_abs_errors(gaul_list = gaul_list,
#                                      df = run_in_oos, ## takes output from get_is_oos_draws()
#                                      sample = ifelse((as.logical(makeholdouts)==TRUE), 'BOTH', "IS"),
#                                      subset_shape = subset_shape,
#                                      ind = indicator,
#                                      ind_gp = indicator_group,
#                                      rd = run_date,
#                                      save.dir = paste0(outputdir, '/abs_error/'))
#
#~~~~~~~~~~~~~#
# End of file #
#~~~~~~~~~~~~~#