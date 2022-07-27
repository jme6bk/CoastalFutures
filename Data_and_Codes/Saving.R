#### all data need to be imported ###

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
Accomack2021 <- Accomack2021 %>% arrange(Crop)
saveRDS(Accomack2021, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Accomack2021.RDS")
