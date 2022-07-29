#### all data need to be imported ###
library(dplyr)
library(ggplot2)
library(sf)

###################################################################
###################################################################
# CDL---------

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

###########################
Northampton2021<-readRDS("/home/jme6bk/github/CoastalFutures/Data_and_Codes/Northampton2021.RDS")
soy_corn <- Northampton2021 %>% filter(Crop == "Corn")
###################

#Accomack CDL
Accomack2021 <- Accomack2021 %>% arrange(Crop)
saveRDS(Accomack2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Accomack2021.RDS")

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

###################################################################
###################################################################
# Black Knight ----------------

#Accomack BlackKnight Housing
saveRDS(AccomackBKlinkedparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/AccomackBKlinkedparcels.RDS")

#Northampton BlackKnight Housing
saveRDS(NorthamptonBKlinkedparcels, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/NorthamptonBKlinkedparcels.RDS")

#install API 
readRenviron("~/.Renviron")
Sys.getenv("USDA_API_KEY")

#Get sf for Accomack and Northhampton
Northampton <- tigris::counties(state = "51", cb = TRUE) %>% 
  st_as_sf() %>% 
  dplyr::filter(NAME %in% "Northampton") 

table(sort(NorthamptonBKlinkedparcels$PTM_ID))

# FOR FINDINGS
ggplot(data = Northampton)+
  geom_sf(data = Northampton) +
  geom_sf(data = NorthamptonBKlinkedparcels, aes(color=PTM_ID, geometry = geometry)) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Top 8 CCAP Data") +
  theme(axis.text.x = element_text(angle=30))


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


