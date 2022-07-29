#Parcel data EDA#

library(sf)
library(dplyr)
library(ggplot2)
library(data.table)

setwd("~/git/dspg22_coastal")

path <- "~/../../../../project/biocomplexity/sdad/projects_data/coastal_futures/dspg2022/"

BKaccomack <- fread(paste0(path,"BlackKnight/accomack_BK_asmt_data.csv"))

BKnorthampton <- fread(paste0(path,"BlackKnight/northampton_BK_asmt_data.csv"))


######################### ACCOMACK ########################
BKaccomack <- BKaccomack %>% select(pid, apn, property_address_latitiude, property_address_longitude,
                                    property_address_census_tract, total_assessed_value, total_market_value)



freq<- sort( table(AccomackBKlinkedparcels$PTM_ID) )
freq<- as.data.frame(freq)
#5A5-4-156, 121E-1-1, 31-A-29A

selected <- ESVAparcels %>% filter(PTM_ID %in% c("5A5-4-156", "121E-1-1", "31-A-29A"))

one <- selected %>% filter(PTM_ID == "5A5-4-156" )
Accomack_houses <- AccomackBKlinkedparcels %>% filter(PTM_ID %in% c("5A5-4-156", "121E-1-1", "31-A-29A"))

one2 <- Accomack_houses %>% filter(PTM_ID == "5A5-4-156")

AccomackBKlinkedparcels<- st_transform(AccomackBKlinkedparcels, crs = st_crs(VA_2_county))
Accomack_houses<- st_transform(Accomack_houses, crs = st_crs(VA_2_county))

ggplot() + 
  geom_sf(data = VA_2_county) +
  geom_sf(data = Accomack_houses) #, aes(color=PTM_ID, geometry=geometry, size = 0.1))

#install API 
readRenviron("~/.Renviron")
Sys.getenv("USDA_API_KEY")

#Get sf for Accomack and Northhampton
VA_2_county <- tigris::counties(state = "51", cb = TRUE) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME %in% "Accomack") 

ggplot(data = selected) + 
  geom_sf(data = Accomack_houses) +
  geom_sf(data = crop_data_001_excluded_v2, aes(color=Crop, geometry=geometry), size=0.01) +
  geom_sf(data = crop_data_002_excluded_v2, aes(color=Crop, geometry=geometry), size=0.01) +
  scale_color_manual(values=col) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Cropland Data Layer")+
  theme(axis.text.x = element_text(angle=30)) +
  guides(colour = guide_legend(override.aes = list(size=1)))

