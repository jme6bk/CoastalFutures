
library(tidycensus)
library(dplyr)
library(ggplot2)
library(viridis)
library(sf)
library(data.table)
library(mipfp)
library(tigris)

################################################################################
################################################################################
#RACE-------------------

# installed Census API key
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_API_KEY")

#### NORTHAMPTON ####

# Load in tracts
Northampton_tracts <- tracts(state = "VA",county = "Northampton") %>% select(GEOID)
Northampton_tracts <- Northampton_tracts %>% filter(GEOID %in% c("51131930100", "51131930200", 
                                                                  "51131930302", "51131930301"))

# make all data sets into SF objects
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Northampton_tracts <- st_as_sf(x = Northampton_tracts,                         
               crs = projcrs)
Northampton_sample <-  st_as_sf(x = NorthamptonsampledfSF,
               crs = projcrs)


sf::sf_use_s2(FALSE)
# Transform CRS and put each person into their respective tract
st_crs(Northampton_sample) <- 4326 
st_crs(Northampton_tracts) <- 4326  
intersect_Northampton <- st_join(Northampton_tracts, Northampton_sample, join = st_intersects)

#Sum the total people in each census tract
Sum_Northampton<-intersect_Northampton%>%
  group_by(GEOID)%>%
  summarise(Count=n())

### sum the type of race for each tract 
tract1 <- intersect_Northampton %>% filter(GEOID %in% "51131930100")
(sort(table(tract1$race)))

tract2 <- intersect_Northampton %>% filter(GEOID %in% "51131930200")
sort(table(tract2$race))

tract3 <- intersect_Northampton %>% filter(GEOID %in% "51131930302")
sort(table(tract3$race))

tract4 <- intersect_Northampton %>% filter(GEOID %in% "51131930301")
sort(table(tract4$race))

#Link a data set 
White <- c(1015, 921, 880, 551)
Black <- c(471, 468, 418, 258)
Two_or_more <- c(14, 20, 16, 12)
Asian <- c(10,12, 13,14)
Other <- c(4,8, 5, 7)
AIAN <- c(1, 0, 1, 0)
Northampton_race <- data.frame(White, Black, Two_or_more, Asian, Other, AIAN)

#combine the two data set and take their percentages
Northampton_plot <- cbind(Sum_Northampton, Northampton_race)

Northampton_plot <- Northampton_plot %>% mutate(Percent_White = (White/Count)*100, Percent_Black = (Black/Count)*100, 
                          Percent_Two_or_More = (Two_or_more/Count)*100, Percent_Asian = (Asian/Count)*100, 
                          Percent_Other = (Other/Count)*100, Percent_AIAN = (AIAN/Count)*100 )


#### ACCOMACK ####

Accomack_tracts <- tracts(state = "VA", county = "Accomack") %>% select(GEOID)
Accomack_tracts <- st_as_sf(Accomack_tracts) %>% filter(GEOID %in% c("51001090300", "51001090800", "51001090600",
                                                                     "51001090500", "51001090700",
                                                                     "51001090202", "51001090402", "51001090401",
                                                                     "51001090201", "51001090102", "51001090101"))

# make all data sets into SF objects
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Accomack_tracts <- st_as_sf(x = Accomack_tracts,                         
                               crs = projcrs)
Accomack_sample <-  st_as_sf(x = AccomacksampledfSF,
                                crs = projcrs)


sf::sf_use_s2(FALSE)
# Transform CRS and put each person into their respective tract
st_crs(Accomack_tracts) <- 4326  
st_crs(Accomack_sample) <- 4326 
intersect_Accomack <- st_join(Accomack_tracts, Accomack_sample, join = st_intersects)

#Sum the total people in each census tract
Sum_Accomack<-intersect_Accomack%>%
  group_by(GEOID)%>%
  summarise(Count=n())

### sum the type of race for each tract 
tract5 <- intersect_Accomack %>% filter(GEOID %in% "51001090101")
(sort(table(tract5$race)))

tract6 <- intersect_Accomack %>% filter(GEOID %in% "51001090102")
sort(table(tract6$race))

tract7 <- intersect_Accomack %>% filter(GEOID %in% "51001090201")
sort(table(tract7$race))

tract8 <- intersect_Accomack %>% filter(GEOID %in% "51001090202")
sort(table(tract8$race))

tract9 <- intersect_Accomack %>% filter(GEOID %in% "51001090300")
sort(table(tract9$race))

tract10 <- intersect_Accomack %>% filter(GEOID %in% "51001090401")
sort(table(tract10$race))

tract11 <- intersect_Accomack %>% filter(GEOID %in% "51001090402")
sort(table(tract11$race))

tract12 <- intersect_Accomack %>% filter(GEOID %in% "51001090500")
sort(table(tract12$race))

tract13 <- intersect_Accomack %>% filter(GEOID %in% "51001090600")
sort(table(tract13$race))

tract14 <- intersect_Accomack %>% filter(GEOID %in% "51001090700")
sort(table(tract14$race))

tract15 <- intersect_Accomack %>% filter(GEOID %in% "51001090800")
sort(table(tract15$race))

#Link a data set 
White <- c(775, 692, 1888, 679, 585, 472, 681, 579, 1051, 1121, 813)
Black <- c(221, 205, 535, 184, 161, 152, 170, 153, 298, 343, 216)
Two_or_more <- c(12, 15, 47, 12, 8, 14, 9, 12, 17, 29, 13)
Asian <- c(6, 4, 16, 6, 4, 2, 5, 8, 13, 11, 2)
Other <- c(2, 0, 4, 1, 0, 1, 1, 1, 2, 1, 4)
AIAN <- c(0, 0, 3, 1, 2, 0, 2, 0, 0, 1, 2)
Accomack_race <- data.frame(White, Black, Two_or_more, Asian, Other, AIAN)

#combine the two data set and take their percentages
Accomack_plot <- cbind(Sum_Accomack, Accomack_race)

Accomack_plot <- Accomack_plot %>% mutate(Percent_White = (White/Count)*100, Percent_Black = (Black/Count)*100, 
                                                Percent_Two_or_More = (Two_or_more/Count)*100, Percent_Asian = (Asian/Count)*100, 
                                                Percent_Other = (Other/Count)*100, Percent_AIAN = (AIAN/Count)*100 )

saveRDS(Accomack_plot, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Accomack_plot.RDS")
saveRDS(Northampton_plot, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/Northampton_plot.RDS")

#Plot
#White
ggplot() +
  geom_sf(Northampton_plot, mapping = aes(geometry = geometry, fill = Percent_White)) +
  geom_sf(Accomack_plot, mapping = aes(geometry = geometry, fill = Percent_White)) +
  ggtitle("Percent Population White by Census Tract") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#Black
ggplot() +
  geom_sf(Northampton_plot, mapping = aes(geometry = geometry, fill = Percent_Black)) +
  geom_sf(Accomack_plot, mapping = aes(geometry = geometry, fill = Percent_Black)) +
  ggtitle("Percent Population Black by Census Tract") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#Asian 
ggplot() +
  geom_sf(Northampton_plot, mapping = aes(geometry = geometry, fill = Percent_Asian)) +
  geom_sf(Accomack_plot, mapping = aes(geometry = geometry, fill = Percent_Asian)) +
  ggtitle("Percent Population Asian by Census Tract") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#Two or more
ggplot() +
  geom_sf(Northampton_plot, mapping = aes(geometry = geometry, fill = Percent_Two_or_More)) +
  geom_sf(Accomack_plot, mapping = aes(geometry = geometry, fill = Percent_Two_or_More)) +
  ggtitle("Percent Population Two or More Races by Census Tract") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) +
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#Other
ggplot() +
  geom_sf(Northampton_plot, mapping = aes(geometry = geometry, fill = Percent_Other)) +
  geom_sf(Accomack_plot, mapping = aes(geometry = geometry, fill = Percent_Other)) +
  ggtitle("Percent Population 'Other' Race by Census Tract") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

################################################################################
################################################################################

#INCOME---------------

#plot 1- change to viridis scale
ggplot() +  
  geom_sf(data = linked_sampled_fSF, size = 0.5, aes(color = income, fill = income, geometry = geometry)) +
  scale_color_viridis(discrete = TRUE, option = "D") +
  scale_fill_viridis(discrete = TRUE) +
  xlab("longitude") + ylab("latitude") + 
  ggtitle("Household Synthetic Population by Income") +
  theme(text = element_text(size = 20)) 


intersect <- rbind(intersect_Northampton, intersect_Accomack)

#Sum total people in each tract
Sum<-intersect%>%
  group_by(GEOID)%>%
  summarise(Count=n())

# find the % of people that make max income in the area
tract1 <- intersect %>% filter(GEOID %in% "51001090101")
(sort(table(tract1$income)))

tract2 <- intersect %>% filter(GEOID %in% "51001090102")
(sort(table(tract2$income)))

tract3 <- intersect %>% filter(GEOID %in% "51001090201")
(sort(table(tract3$income)))

tract4 <- intersect %>% filter(GEOID %in% "51001090202")
(sort(table(tract4$income)))

tract5 <- intersect %>% filter(GEOID %in% "51001090300")
(sort(table(tract5$income)))

tract6 <- intersect %>% filter(GEOID %in% "51001090401")
(sort(table(tract6$income)))

tract7 <- intersect %>% filter(GEOID %in% "51001090402")
(sort(table(tract7$income)))

tract8 <- intersect %>% filter(GEOID %in% "51001090500")
(sort(table(tract8$income)))

tract9 <- intersect %>% filter(GEOID %in% "51001090600")
(sort(table(tract9$income)))

tract10 <- intersect %>% filter(GEOID %in% "51001090700")
(sort(table(tract10$income)))

tract11 <- intersect %>% filter(GEOID %in% "51001090800")
(sort(table(tract11$income)))

tract12 <- intersect %>% filter(GEOID %in% "51131930100")
(sort(table(tract12$income)))

tract13 <- intersect %>% filter(GEOID %in% "51131930200")
(sort(table(tract13$income)))

tract14 <- intersect %>% filter(GEOID %in% "51131930301")
(sort(table(tract14$income)))

tract15 <- intersect %>% filter(GEOID %in% "51131930302")
(sort(table(tract15$income)))

income_75_100 <- c(139, 132, 331, 105, 90, 77, 117, 91, 166, 221, 118, 198, 188, 168, 118)
income_less_than_10 <- c(55, 50, 139, 66, 38, 37, 51, 49, 90, 71, 59, 128, 126, 110, 69)
income_more_than_200 <- c(24, 18, 71, 29, 17, 14, 17, 22, 25, 34, 26, 64, 71, 61, 41)

#Link to Sum dataframe and get percentage column
plot <- cbind(Sum, income_75_100, income_less_than_10, income_more_than_200)

plot_most_frequent_income <- plot %>% mutate(Percent_of_Population = (income_75_100/Count)*100)
plot_lowest_income <-  plot %>% mutate(Percent_of_Population = (income_less_than_10/Count)*100)
plot_highest_income <- plot %>% mutate(Percent_of_Population = (income_more_than_200/Count)*100)

saveRDS(plot_most_frequent_income, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/plot_most_frequent_income.RDS")
saveRDS(plot_lowest_income, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/plot_lowest_income.RDS")
saveRDS(plot_highest_income, file = "/home/jme6bk/github/CoastalFutures/Data_and_Codes/plot_highest_income.RDS")

#plot 2- most frequenct in 75,000-99,999 distrubution
ggplot() +
  geom_sf(plot, mapping = aes(geometry = geometry, fill = Percent_of_Population)) +
  ggtitle("Percent Distribution of Population making $75,000-$99,999") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#plot 2- lowest income distrubution
ggplot() +
  geom_sf(plot_lowest_income, mapping = aes(geometry = geometry, fill = Percent_of_Population)) +
  ggtitle("Percent Distribution of Population making Less than $10,000") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")

#plot 2- highest income distrubution
ggplot() +
  geom_sf(plot_highest_income, mapping = aes(geometry = geometry, fill = Percent_of_Population)) +
  ggtitle("Percent Distribution of Population more than $200,000") + xlab('Longitude') + ylab("Latitude") +
  theme(text = element_text(size = 20)) + 
  scale_fill_continuous(high = "#132B43", low = "#56B1F7")


# make income a continous variable

# breaks <- c("Less than 10,000", "10,000-14,999","15,000-19,999", "20,000-24,999","25,000-29,999",
#             "30,000-34,999","35,000-39,999","40,000-44,999", "45,000-49,999", "50,000-59,999", 
#             "60,000-74,999 "," 75,000-99,999 ", "100,000-124,999", 
#             "125,000-149,999", "150,000-199,999", "200,000 or more")

linked_sampled_fSF <- rbind(AccomacksampledfSF, NorthamptonsampledfSF)


linked_gsub <- linked_sampled_fSF %>% mutate(income1 = gsub("Less than 10,000", 8000, income))
linked_gsub <- linked_gsub %>% mutate(income2 = gsub("10,000-14,999", 12000, income1))
linked_gsub <- linked_gsub %>% mutate(income3 = gsub("15,000-19,999", 17000, income2))
linked_gsub <- linked_gsub %>% mutate(income4 = gsub("20,000-24,999", 22000, income3))
linked_gsub <- linked_gsub %>% mutate(income5 = gsub("25,000-29,999", 27000, income4))
linked_gsub <- linked_gsub %>% mutate(income6 = gsub("30,000-34,999", 32000, income5))
linked_gsub <- linked_gsub %>% mutate(income7 = gsub("35,000-39,999", 37000, income6))
linked_gsub <- linked_gsub %>% mutate(income8 = gsub("40,000-44,999", 42000, income7))
linked_gsub <- linked_gsub %>% mutate(income9 = gsub("45,000-49,999", 47000, income8))
linked_gsub <- linked_gsub %>% mutate(income10 = gsub("50,000-59,999", 55000, income9))
linked_gsub <- linked_gsub %>% mutate(income11 = gsub("60,000-74,999", 68000, income10))
linked_gsub <- linked_gsub %>% mutate(income12 = gsub("75,000-99,999", 85000, income11))
linked_gsub <- linked_gsub %>% mutate(income13 = gsub("100,000-124,999", 115000, income12))
linked_gsub <- linked_gsub %>% mutate(income14 = gsub("125,000-149,999", 135000, income13))
linked_gsub <- linked_gsub %>% mutate(income15 = gsub("150,000-199,999", 175000, income14))
linked_gsub <- linked_gsub %>% mutate(Incomes = gsub("200,000 or more", 205000, income15))

#Check
table(linked_gsub$Incomes)

linked_gsub$Incomes <- as.numeric(as.character(linked_gsub$Incomes))

#Plot 3 - as a continous variable
ggplot() +  
  geom_sf(data = linked_gsub, size = 0.5, aes(color = Incomes, geometry = geometry)) +
  xlab("Longitude") + ylab("Latitude") + ggtitle("Household Synthetic Population by Income") +
  theme(text = element_text(size = 20)) +
  scale_color_continuous(high = "#132B43", low = "#56B1F7")

  
  
      
