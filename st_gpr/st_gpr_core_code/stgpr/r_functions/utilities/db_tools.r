###########################################################
### Author: Patrick Liu
### Date: 1/26/2015
### Project: ST-GPR
### Purpose: Database utility functions
###########################################################

###################
### Setting up ####
###################
library(data.table)
library(stringr)
library(plyr)
library(ini)
library(RMySQL)
library(parallel)

####################################################################################################################################################
# 															   Table of Contents
####################################################################################################################################################

## Base
	## query

## Pulls
  ## get_best_lvid
  ## get_location_hierarchy
  ## get_gbd_round_id

####################################################################################################################################################
# 																	 Base
####################################################################################################################################################

gbd_2020_decomp_step_id <- list()
gbd_2020_decomp_step_id['step1'] <- 16
gbd_2020_decomp_step_id['step2'] <- 17
gbd_2020_decomp_step_id['step3'] <- 18
gbd_2020_decomp_step_id['step4'] <- 19
gbd_2020_decomp_step_id['step5'] <- 20
gbd_2020_decomp_step_id['iterative'] <- 15

gbd_2019_decomp_step_id <- list()
gbd_2019_decomp_step_id['step1'] <- 1
gbd_2019_decomp_step_id['step2'] <- 2
gbd_2019_decomp_step_id['step3'] <- 3
gbd_2019_decomp_step_id['step4'] <- 4
gbd_2019_decomp_step_id['step5'] <- 5
gbd_2019_decomp_step_id['iterative'] <- 7
gbd_2019_decomp_step_id['usa_re'] <- 14

decomp_step_id_from_decomp_step <- function(step, gbd_round_id){
  if (gbd_round_id == 7){
    step_map <- gbd_2020_decomp_step_id
  } else if (gbd_round_id == 6) {
    step_map <- gbd_2019_decomp_step_id
  } else if (gbd_round_id == 5) {
    return(9)
  } else if (gbd_round_id == 4) {
    return(10)
  } else if (gbd_round_id == 3) {
    return(11)
  } else if (gbd_round_id == 2) {
    return(12)
  }
  return(as.integer(step_map[step]))
}

query <- function(query, conn_def) {
  cluster_obdc_path <- "/ihme/cc_resources/credentials/.odbc.ini"
  odbc <- if (file.exists(cluster_obdc_path)) {
      read.ini(cluster_obdc_path)
    } else {
      return(read.ini("/home/j/temp/central_comp/credentials/.odbc.ini"))
    }
  conn <- dbConnect(RMySQL::MySQL(), 
                    host = odbc[[conn_def]]$server, 
                    username = odbc[[conn_def]]$user, 
                    password = odbc[[conn_def]]$password)
  dt <- dbGetQuery(conn,query) %>% data.table
  dbDisconnect(conn)
  return(dt)
}

####################################################################################################################################################
# 																	 Pulls
####################################################################################################################################################

get_best_lvid <- function(location_set_id, gbd_round_id, decomp_step = NULL){
  host   <- "cod"
  
  decomp_step_id <- if (is.null(decomp_step)) {
    "NULL" 
  } else {
    decomp_step_id_from_decomp_step(decomp_step, gbd_round_id)
  }
  
  q <- paste0("SELECT shared.active_location_set_decomp_step_version(", 
              location_set_id, ",", gbd_round_id, ",", decomp_step_id, ")")
  location_set_best <- query(q, host) %>% unlist %>% unname
  
  return(location_set_best)
}

# This is 10x faster than the shared function version and I'm not sure
# how often it's used/called so I don't want to make big performance changes
# right away
get_location_metadata <- function(location_set_id, gbd_round_id, decomp_step = NULL) {
  host   <- "cod"
  location_set_version_id <- get_best_lvid(location_set_id = location_set_id, gbd_round_id = gbd_round_id,
                                           decomp_step = decomp_step)
  
  q <- sprintf('SELECT * FROM shared.location_hierarchy_history WHERE location_set_version_id=%i', 
               location_set_version_id)
  df <- query(q, host)
  
  # assert existing location_set
  if (nrow(df) == 0) {
    stop("Locations dataframe is empty! Make sure your location_set_id and gbd_round_id are legit.")
  }

  return(df[])
}

# Vectorizable function that returns 1 if a location_id is a standard location; 0 otherwise
# Since standard locations didn't exist before GBD 2019, earlier rounds are handled differently
is_standard_location <- function(location_id, level, gbd_round_id, standard_location_ids) {
  if (gbd_round_id > 5) {
    return(as.integer(location_id %in% standard_location_ids))
  } else {
    old_standard_locs <- c(4749, 4636, 434, 433)
    return(as.integer(level == 3 | location_id %in% old_standard_locs))
  }
}

get_location_hierarchy <- function(location_set_id = 22, gbd_round_id = 7, decomp_step = NULL, standard_location_set_id = 101) {
  # pull best location_set_version_id from location_set_version_active and 
  # location hierarchy based on best location_set_version_id for given hierarchy and GBD round
  df <- get_location_metadata(location_set_id = location_set_id, gbd_round_id = gbd_round_id,
	                              decomp_step = decomp_step)
	
  std_locs <- if (gbd_round_id > 5) {
	  get_location_metadata(location_set_id = standard_location_set_id, gbd_round_id = gbd_round_id)[, location_id]
	} else {
	  NULL
	}

	# Create hierarchy
	hierarchy <- str_split_fixed(df$path_to_top_parent, ",", max(df$level) + 1) %>% data.table
	hierarchy <- hierarchy[, lapply(.SD, as.numeric)]
	setnames(hierarchy, names(hierarchy), paste0("level_", seq(0, max(df$level))))
	df <- cbind(df, hierarchy)
	
	# Create indicator column for standard locations, using the janky setup from GBD2017 for all rounds before GBD2019
	df[, standard_location := mapply(is_standard_location, location_id, level,
	                                 MoreArgs = list(gbd_round_id = gbd_round_id, standard_location_ids = std_locs))]
	
	return(df[])
}

#####################################################################################################################################################

get_gbd_round_id <- function(location_set_version_id){
  # This function is used to pull old (GBD 2017 and before) ST-GPR data.
  # It should be deleted when there is a new GPR Viz.
  
  #Databaser stuff
  host   <- "cod"
  
  ## Find active version id if not specified
  q <- paste0("SELECT gbd_round FROM shared.location_set_version WHERE location_set_version_id = ", location_set_version_id)
  gbd_round <- query(q, host)
  
  #Re-query to match gbd_round (only available thing) to gbd_round_id
  q <- paste0("SELECT gbd_round_id FROM shared.gbd_round WHERE gbd_round = ", gbd_round)
  gbd_round_id <- query(q, host) %>% as.integer()
  
  return(gbd_round_id)
}
