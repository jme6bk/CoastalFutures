#Parcel data EDA#

library(sf)
library(data.table)

setwd("~/git/dspg22_coastal")

path <- "~/../../../../project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/"

BKaccomack <- fread(paste0(path,"BlackKnight/accomack_BK_asmt_data.csv"))

BKnorthampton <- fread(paste0(path,"BlackKnight/northampton_BK_asmt_data.csv"))


######################### ACCOMACK ########################
BKaccomack <- BKaccomack %>% select(pid, apn, property_address_latitiude, property_address_longitude,
                                    property_address_census_tract, total_assessed_value, total_market_value)

Geo_Accomack_BK <- read.csv(file = "/project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/BlackKnight/accomack_BK_asmt_data.csv")
