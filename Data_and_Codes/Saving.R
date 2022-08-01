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
saveRDS(corn_accomack, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/corn_accomack2.RDS")

#Merge_corn_accomack 1 (without parcel area)
saveRDS(merge_corn_accomack, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/merge_corn_accomack1.RDS")

#Merge_corn_accomack 3 (with parcel area and percent column)
saveRDS(merge_corn_accomack, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/merge_corn_accomack2.RDS")

#Northampton CDL
Northampton2021 <- Northampton2021 %>% arrange(Crop)
saveRDS(Northampton2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Northampton2021.RDS")

#Accomack CDL
saveRDS(Accomack2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Accomack2021.RDS")

########################### graphs #######################
# Northampton graph
sample_parcels <- Northampton2016 %>% filter(PTM_ID %in% c("69-A-8", "105-9-E", "77B-1-1"))

ggplot(sample_parcels, aes(fill=Crop, y=perc_area_crop, x=PTM_ID)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200", "orange", "white")) +
  xlab("Parcel ID") + ylab("Percent Area Crop") + ggtitle("Percent Area Crop per Parcel") +
  theme(text = element_text(size = 20))    

### Accomack graph ###
sample_parcels <- Accomack2016 %>% filter(PTM_ID %in% c("109-2-A", "25-A-100", "112-A-5"))

#CDL Parcels Plot- Accomack
ggplot(sample_parcels, aes(fill=Crop, y=perc_area_crop, x=PTM_ID)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","#e6a01d","#e57200", "orange", "white")) +
  xlab("Parcel ID") + ylab("Percent Area Crop") + ggtitle("Percent Area Crop per Parcel") +
  theme(text = element_text(size = 20))    

###################################################################
###################################################################
# CCAP --------
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

#CDL Parcels Plot- Northampton
ggplot(sample_landuse_Northampton, aes(fill=use, y=perc_area_landcover, x=PTM_ID)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values = c("#232d4b","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#d9e12b","#e6ce3a","#e6a01d",
                               "#e57200", "orange", "white","#d62828", 
                               "#ffbe0b", "#003566", "#ffd60a")) +
  xlab("Parcel ID") + ylab("Percent Area Landcover") + ggtitle("Percent Area Landcover per Parcel") +
  theme(text = element_text(size = 20))    

#Accomack
sample_landuse <- landuse2016 %>% filter(PTM_ID %in% c("109-2-A", "25-A-100", "112-A-5"))
sample_landuse_Accomack <- sample_landuse %>% filter(LOCALITY=="Accomack County")


#CDL Parcels Plot- Northampton
ggplot(sample_landuse_Accomack, aes(fill=use, y=perc_area_landcover, x=PTM_ID)) + 
  geom_bar(position="dodge", stat="identity") +
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
ggplot()+

saveRDS(microdata, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/microdata.RDS")

###################################################################
###################################################################
#Parcel data -------------
saveRDS(ESVAparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/ESVAparcels.RDS")

###################################################################
###################################################################
#IPF OUT PUT
saveRDS(Linkeddfs, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Total_IPF_Output.RDS")

saveRDS(NorthamptonsampledfSF, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/NorthamptonsampledfSF.RDS")

saveRDS(dfx, file =  "/home/jme6bk/github/CoastalFutures/Data_and_Codes/freq_IPF_output.RDS")

saveRDS(AccomacksampledfSF, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/AccomacksampledfSF.RDS")

VA_2_county <- tigris::counties(state = "51", cb = TRUE) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME %in% c("Accomack", "Northampton")) 

## Race ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = race, geometry = geometry), size = 0.5) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = race, geometry = geometry), size = 0.5) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Household Synthetic Population by Race")

### Bar graph for Race

linked_sampled_fSF <- rbind(AccomacksampledfSF, NorthamptonsampledfSF)

ggplot(linked_sampled_fSF, aes(x = race, fill = tenure)) + 
  geom_bar(fill = c("#E57200","#2c4f6b","#0e879c","#60999a",
                               "#d1e0bf","#E6CE3A")) +
  xlab("Race") + ylab("Count") + ggtitle("Synthetic Household by Race") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) 
  


## Income ##

ggplot(VA_2_county) +
  geom_sf(data = VA_2_county) +
  geom_sf(data = AccomacksampledfSF, aes(color = income, geometry = geometry), size = 0.5) +
  geom_sf(data = NorthamptonsampledfSF, aes(color = income, geometry = geometry), size = 0.5) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Household Synthetic Population by Income")


ggplot(linked_sampled_fSF, aes(x = income, fill = tenure)) + 
  geom_bar() +
  xlab("Income") + ylab("Count") + ggtitle("Synthetic Household by Income") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) +
  scale_fill_manual(values = c("#2c4f6b","#e57200")) 

ggplot(linked_sampled_fSF, aes(x = income, fill = age)) + 
  geom_bar() +
  xlab("Income") + ylab("Count") + ggtitle("Synthetic Household by Income") +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x = element_text(angle=15)) 
