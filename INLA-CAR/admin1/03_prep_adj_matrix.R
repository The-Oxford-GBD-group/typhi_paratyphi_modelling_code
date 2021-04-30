library(data.table)
library(spdep)
library(sf)
rm(list = ls())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# Prep the adjacency matrix and data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
mydata <- fread('Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/MDR_Typhi.csv')
all_covs <- fread('Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv')

my_shapefile <- st_read('c:/Users/annieb/Desktop/admin2013_1.shp')
typhi_endemic <- fread("Z:/AMR/Pathogens/typhi_paratyphi/misc/typhi_endemic_locations2.csv")
typhi_endemic <- typhi_endemic[typhi_endemic$level == 3,]

#remove CPV, SYC, MDC, BHR
typhi_endemic <- typhi_endemic[typhi_endemic$ihme_lc_id!='MDV',]
typhi_endemic <- typhi_endemic[typhi_endemic$ihme_lc_id!='SYC',]
typhi_endemic <- typhi_endemic[typhi_endemic$ihme_lc_id!='BHR',]
typhi_endemic <- typhi_endemic[typhi_endemic$ihme_lc_id!='CPV',]
typhi_endemic <- typhi_endemic[typhi_endemic$ihme_lc_id!='SGP',]
typhi_endemic <- typhi_endemic[,.(ihme_lc_id, spr_reg_id)]

#sort shapefile by super-region and locs
                                   
my_shapefile <- merge(my_shapefile, typhi_endemic, by.x = 'COUNTRY_ID', by.y = 'ihme_lc_id')
my_shapefile <- my_shapefile[order(my_shapefile$spr_reg_id, my_shapefile$COUNTRY_ID, my_shapefile$GAUL_CODE),]

#make adjacent IDs for all locs, just africa and then all others
my_shapefile$adj_id <- 1:nrow(my_shapefile)
my_shapefile$adj_id_sSA[my_shapefile$spr_reg_id == 166] <-  1:nrow(my_shapefile[my_shapefile$spr_reg_id == 166,])
my_shapefile$adj_id_Asia[my_shapefile$spr_reg_id != 166] <-  1:nrow(my_shapefile[my_shapefile$spr_reg_id != 166,])

all_covs <- all_covs[all_covs$COUNTRY_ID!='MDV',]
all_covs <- all_covs[all_covs$COUNTRY_ID!='SYC',]
all_covs <- all_covs[all_covs$COUNTRY_ID!='BHR',]
all_covs <- all_covs[all_covs$COUNTRY_ID!='CPV',]
all_covs <- all_covs[all_covs$COUNTRY_ID!='SGP',]

shp.adj <- poly2nb(my_shapefile,queen=TRUE, row.names=my_shapefile$adj_id)
shp.adj.sSA <- poly2nb(my_shapefile[!is.na(my_shapefile$adj_id_sSA),],queen=TRUE, row.names=my_shapefile$adj_id_sSA[!is.na(my_shapefile$adj_id_sSA)])
shp.adj.Asia <- poly2nb(my_shapefile[!is.na(my_shapefile$adj_id_Asia),],queen=TRUE, row.names=my_shapefile$adj_id_sSA[!is.na(my_shapefile$adj_id_Asia)])

#Convert the adjacency matrix into a file in the INLA format and save
nb2INLA("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/typhi_adj.adj", shp.adj)
nb2INLA("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/typhi_adj_sSA.adj", shp.adj.sSA)
nb2INLA("Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/typhi_adj_Asia.adj", shp.adj.Asia)

adj_lookup <- my_shapefile[c('GAUL_CODE', 'adj_id', 'adj_id_sSA', 'adj_id_Asia')]
adj_lookup <- data.frame(adj_lookup)
adj_lookup$geometry <-  NULL

mydata <- merge(mydata, adj_lookup, by.x = 'adm1', by.y = 'GAUL_CODE')
all_covs <- merge(all_covs, adj_lookup, by.x = 'admin_code', by.y = 'GAUL_CODE')

write.csv(mydata, 'Z:/AMR/Pathogens/typhi_paratyphi/model_prep/admin1_ST_CAR/MDR_Typhi.csv', row.names = F)
write.csv(all_covs, 'Z:/AMR/Covariates/modelling_covariates/admin1_typhi/all_admin1_typhi_covs.csv', row.names = F)
