#### all data need to be imported ###
library(dplyr)
library(ggplot2)
library(sf)
library(units)

###################################################################
###################################################################
# CDL---------
#orginal cdl
saveRDS(cdl_sf_Accomack, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/cdl_accomack.RDS")

#corn_Accomack1
saveRDS(corn_accomack, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/corn_accomack1.RDS")

#Corn_accomack2
corn_accomack2 <- corn_accomack2 %>% mutate(across(where(is.numeric), round, 2))
saveRDS(corn_accomack2, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/corn_accomack2.RDS")

#Merge_corn_accomack 1 (without parcel area)
merge_corn_accomack1 <- merge_corn_accomack1 %>% mutate(across(where(is.numeric), round, 2))
saveRDS(merge_corn_accomack1, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/merge_corn_accomack1.RDS")

#Merge_corn_accomack 3 (with parcel area and percent column)
merge_corn_accomack2 <- merge_corn_accomack2 %>% mutate(across(where(is.numeric), round, 2))
saveRDS(merge_corn_accomack2, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/merge_corn_accomack2.RDS")

#Northampton CDL
Northampton2021 <- Northampton2021 %>% arrange(Crop) %>% mutate(across(where(is.numeric), round, 2))
saveRDS(Northampton2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Northampton2021.RDS")

#Accomack CDL
saveRDS(Accomack2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Accomack2021.RDS")

########################### graphs #######################
# Northampton graph
sample_parcels <- Northampton2016 %>% filter(PTM_ID %in% c("69-A-8", "105-9-E", "77B-1-1"))

ggplot(sample_parcels, aes(fill=Crop, y=perc_area_crop, x=PTM_ID)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","white","#e6a01d","#e57200", "orange")) +
  xlab("Parcel ID") + ylab("Percent Area Crop") + ggtitle("Percent Area Crop per Parcel") +
  theme(text = element_text(size = 20))    

### Accomack graph ###
sample_parcels <- Accomack2016 %>% filter(PTM_ID %in% c("78A1-1-208", "25-A-100", "112-A-5"))

#CDL Parcels Plot- Accomack
ggplot(sample_parcels, aes(fill=Crop, y=perc_area_crop, x=PTM_ID)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","white","#e6a01d","#e57200", "orange")) +
  xlab("Parcel ID") + ylab("Percent Area Crop") + ggtitle("Percent Area Crop per Parcel") +
  theme(text = element_text(size = 20))    

###################################################################
###################################################################
# CCAP --------
landuse2016 <- landuse2016 %>% mutate(across(where(is.numeric), round, 2))
saveRDS(landuse2016, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/landuse2016.RDS")
cover <- ccap2016 %>% filter(landcover %in% c(8,2,16,17,22,7,15,3))
top_cover <- cover %>% mutate(Use = ifelse(landcover==8, "Grassland",
                                           ifelse(landcover==2, "High Intensity Developed",
                                                  ifelse(landcover==16, "Estuarine Forested Wetland", 
                                                         ifelse(landcover==17, "Estuarine Scrub/Shrub Wetland",
                                                                ifelse(landcover==22, "Palustrine Aquatic Bed",
                                                                       ifelse(landcover==7, "Pasture/Hay", 
                                                                              ifelse(landcover==15, "Palustrine Emergent Wetland",
                                                                                     "Medium Intensity Developed"))))))))

library(RColorBrewer)
n <- 8
colrs <- brewer.pal.info[brewer.pal.info$colorblind == TRUE, ]
col_vec = unlist(mapply(brewer.pal, colrs$maxcolors, rownames(colrs)))
col <- sample(col_vec, n)

ggplot() +
  geom_sf(data = top_cover, aes(color=Use, geometry = geometry)) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Top 8 CCAP Data") +
  theme(axis.text.x = element_text(angle=30)) +
  guides(colour = guide_legend(override.aes = list(size=1)))    

############### Graph ###################
#Northampton
sample_landuse <- landuse2016 %>% filter(PTM_ID %in% c("69-A-8", "105-9-E", "77B-1-1"))
sample_landuse_Northampton <- sample_landuse %>% filter(LOCALITY=="Northampton County")

#CCA Parcels Plot- Northampton
ggplot(sample_landuse_Northampton, aes(fill=use, y=perc_area_landcover, x=PTM_ID)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","#e6a01d",
                               "#e57200", "orange", "white","#d62828", 
                               "#ffbe0b", "#003566", "#ffd60a")) +
  xlab("Parcel ID") + ylab("Percent Area Landcover") + ggtitle("Percent Area Landcover per Parcel") +
  theme(text = element_text(size = 20))    

#Accomack
sample_landuse <- landuse2016 %>% filter(PTM_ID %in% c("78A1-1-208", "25-A-100", "112-A-5"))
sample_landuse_Accomack <- sample_landuse %>% filter(LOCALITY=="Accomack County")


#CCAP Parcels Plot- Accomack
ggplot(sample_landuse_Accomack, aes(fill=use, y=perc_area_landcover, x=PTM_ID)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","#e6a01d",
                               "#e57200", "orange", "white","#d62828", 
                               "#ffbe0b", "#003566", "#ffd60a")) +
  xlab("Parcel ID") + ylab("Percent Area Landcover") + ggtitle("Percent Area Landcover per Parcel") +
  theme(text = element_text(size = 20))    

###################################################################
###################################################################
# Black Knight ----------------

#Accomack BlackKnight Housing
saveRDS(AccomackBKlinkedparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/AccomackBKlinkedparcels.RDS")

#Northampton BlackKnight Housing
saveRDS(NorthamptonBKlinkedparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/NorthamptonBKlinkedparcels.RDS")


###################################################################
###################################################################
#IPF -----------

saveRDS(cleaned_pums, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/cleaned_pums.RDS")
saveRDS(Northampton930100, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Northampton930100.RDS")

microdata <- as.data.frame(microdata_table) %>% rename(Age = Var1, Income = Var2, Race = Var3, Tenure = Var4)

saveRDS(microdata, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/microdata.RDS")

###################################################################
###################################################################
#Parcel data -------------

ESVAparcels <- ESVAparcels %>% mutate(across(where(is.numeric), round, 2))
saveRDS(ESVAparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/ESVAparcels.RDS")

###################################################################
###################################################################
#IPF FINDINGS
saveRDS(Linkeddfs, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Total_IPF_Output.RDS")

# Manipulating Geometry for display
coords <- st_coordinates(NorthamptonsampledfSF$geometry)
class(coords)
coords <- data.frame(coords)
coords <- coords %>% mutate(across(where(is.numeric), round, 2)) %>% rename(Latitude = X, Longitude = Y)
NorthamptonsampledfSF_Coords <- NorthamptonsampledfSF %>% select(!(c(geometry, pid)))
NorthamptonsampledfSF_Coords <- cbind(NorthamptonsampledfSF_Coords, coords)
saveRDS(NorthamptonsampledfSF_Coords, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/NorthamptonsampledfSF_Coords.RDS")

saveRDS(dfx, file =  "/home/jme6bk/github/CoastalFutures/Data_and_Codes/freq_IPF_output.RDS")

# Manipulating Geometry for display
coords <- st_coordinates(AccomacksampledfSF$geometry)
class(coords)
coords <- data.frame(coords)
coords <- coords %>% mutate(across(where(is.numeric), round, 2)) %>% rename(Latitude = X, Longitude = Y)
AccomacksampledfSF_Coords <- AccomacksampledfSF %>% select(!(c(geometry, pid)))
AccomacksampledfSF_Coords <- cbind(AccomacksampledfSF_Coords, coords)
saveRDS(AccomacksampledfSF_Coords, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/AccomacksampledfSF_Coords.RDS")

VA_2_county <- tigris::counties(state = "51", cb = TRUE) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME %in% c("Accomack", "Northampton")) 

  linked_sampled_fSF <- rbind(AccomacksampledfSF, NorthamptonsampledfSF)

## Race ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = race, geometry = geometry), size = 1) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = race, geometry = geometry), size = 1) +
  xlab("longitude") + ylab("latitude") + 
  theme(text = element_text(size = 20)) +
  ggtitle("Household Synthetic Population by Race")

## Income ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = income, geometry = geometry), size = 1) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = income, geometry = geometry), size = 1) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Household Synthetic Population by Income") +
  theme(text = element_text(size = 20)) 

## AGE ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = age, geometry = geometry), size = .5) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = age, geometry = geometry), size = .5) +
  xlab("longitude") + ylab("latitude") + 
  theme(text = element_text(size = 20)) +
  ggtitle("Household Synthetic Population by Age") +
  scale_color_viridis(discrete = TRUE, option = "D") 

## TENURE ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = tenure, geometry = geometry), size = 1) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = tenure, geometry = geometry), size = 1) +
  xlab("longitude") + ylab("latitude") + 
  theme(text = element_text(size = 20)) +
  ggtitle("Household Synthetic Population by Tenure")

### Bar graphs - race, income, age, tenure ##

#Race by Tenure
ggplot(linked_sampled_fSF, aes(x = race, fill = tenure)) + 
  geom_bar() +
  scale_fill_manual(values = c("#2c4f6b","#e57200")) +
  xlab("Race") + ylab("Count") + ggtitle("Synthetic Household by Race and Tenure") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) 

#Income by age
ggplot(linked_sampled_fSF, aes(x = income, fill = age)) + 
  geom_bar() +
  xlab("Income") + ylab("Count") + ggtitle("Synthetic Household by Income and Age") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) +
  scale_fill_manual(values = c("#2c4f6b","#e57200", "#D1E0BF", "#0E879C")) 

#income by race
ggplot(linked_sampled_fSF, aes(x = income, fill = race)) + 
  geom_bar() +
  xlab("Income") + ylab("Count") + ggtitle("Synthetic Household by Income and Race") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) +
  scale_fill_manual(values = c("#2c4f6b","#e57200", "#D1E0BF", "#0E879C","#E6CE3A", "#E6A01D")) 

#Age by race
ggplot(linked_sampled_fSF, aes(x = age, fill = race)) + 
  geom_bar() +
  xlab("age") + ylab("Count") + ggtitle("Synthetic Household by Race and Age") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) +
  scale_fill_manual(values = c("#2c4f6b","#e57200", "#D1E0BF", "#0E879C","#E6CE3A", "#E6A01D")) 
