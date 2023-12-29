
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(dplyr)
library(stringdist)
library(readxl)

#reads in dataframes extracted from inaturalist from parks (2019-01-01 to 2023-12-20)
#combines them into a single dataframe 

setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project")



##########################.
#### read in datasets ####
###########################.

# read in data
birds<-read_excel("Rdata/SurveyData/2023-12-23_BirdSurveys.xlsx")

#get download date from filename
file_name <- data.frame(file=basename("Rdata/SurveyData/2023-12-23_BirdSurveys.xlsx"))
file_name <- file_name %>% 
  separate(file, into = c("date","type"), sep = "_") %>%
  separate(type, into=c("type", "drop"), sep=".xlsx")

# add download date
birds$downloaded <- file_name$date

# change column names
birds$park <- birds$Site
birds$park[which(birds$park=="CrotonaPark")]<-"Crotona"
birds$park[which(birds$park=="InwoodHillPark")]<-"InwoodHill"
birds$park[which(birds$park=="InnwoodHillPark")]<-"InwoodHill"
birds$park[which(birds$park=="MorningsidePark")]<-"Morningside"
birds$park[which(birds$park=="PelhamBayPark")]<-"PelhamBaySouth"
birds$park[which(birds$park=="SoundviewPark")]<-"Soundview"
birds$park[which(birds$park=="VanCortlandtPark")]<-"VanCortlandt"
birds$park[which(birds$park=="HighbridgePark")]<-"Highbridge"







##### search for typos ####

species <- data.frame(scientific_name=unique(birds$Species))
    
#Function to find fuzzy matches
find_fuzzy_matches <- function(x, strings) {
  stringdist::stringdistmatrix(x, strings, method = "jw")  # Using Jaro-Winkler distance
}

# Apply the function to find fuzzy matches for each string in the column
matches <- apply(species, 1, function(row) find_fuzzy_matches(row, species$scientific_name))

below_threshold <- which(matches < 0.1, arr.ind = TRUE)

# Print the pairs and their distances
result_df <- data.frame(String1 = character(), String2 = character(), Distance = numeric(), stringsAsFactors = FALSE)
for (i in 1:nrow(below_threshold)) {
  row_idx <- below_threshold[i, 1]
  col_idx <- below_threshold[i, 2]
  distance <- matches[row_idx, col_idx]
  result_df <- rbind(result_df, c(species$scientific_name[row_idx], species$scientific_name[col_idx], distance))
}


# visually inspect list
# change typos
# remove subspecies for those with duplicates when one does not have subspecies

birds$Species[which(birds$Species=="OnLeashDog")] <-  "DogOnLeash"
birds$Species[which(birds$Species=="OffLeashDog")] <-  "DogOffLeash"
birds$Species[which(birds$Species=="Woodduck")] <-  "WoodDuck"
birds$Species[which(birds$Species=="BlackcappedChickadee")] <-  "BlackCappedChickadee"
birds$Species[which(birds$Species=="GreenwingedTeal")] <-  "GreenWingedTeal"
birds$Species[which(birds$Species=="RedwingedBlackbird")] <-  "RedWingedBlackbird"
birds$Species[which(birds$Species=="mourningDove")] <-  "MourningDove"
birds$Species[which(birds$Species=="whiteThroatedSparrow")] <-  "WhiteThroatedSparrow"


#### calculate richness  ####
### also accounting for total observations ###

#by group
bird_richness <- birds %>%
  group_by(park) %>%
  summarize(unique_sp = length(unique(Species)))


#### Consolidate with other datasets ####

#Add SVI and crime data (with size of park)
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
cencus_data_summary<-cencus_data %>% group_by(park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))  
cencus_data_summary$park[which(cencus_data_summary$park=="Inwood")]<-"InwoodHill"

crime <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/park_crime_21_23.csv") 
crime <- crime %>% select(Park, Acres, crime_total) %>%
  mutate(park=Park, crime_per_acre=(crime_total/Acres))
crime$park[which(crime$park=="Inwood")]<-"InwoodHill"


#combine
combo<-left_join(bird_richness, cencus_data_summary, by="park")
combo<-left_join(combo, crime, by="park") %>%
  mutate(sp_rich_area=(unique_sp/Acres))


                 
#correct for # of observations



#Plots
combo<-combo[-which(combo$park=="CentralPark"),]


ggplot(data=combo, aes(x=reorder(park, SVI), y=sp_rich_area, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Species Richness / area") +
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  xlab("")+
  ggtitle("Bird Surveys") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#ggsave(filename="birds_svi_bars.jpg", width = 5, height = 7, units = c("in"), dpi = 300, 
#       path = "~/Documents/Projects/LuxuryNYC/Figures")





#colored by park
morningside= "#8d5a99"
inwood="#a47158"
vancortlandt="#e8718d"
soundview="#097d79"
pelham="#e5b636"
highbridge="#e77148"
crotona="#54a82d"
                
parkcolors<-c(morningside, inwood, crotona, highbridge, vancortlandt, soundview, pelham)

ggplot(data=combo, aes(x=SVI, y=sp_rich_area, color=park, shape=taxon)) +
  geom_point(aes(x=SVI, y=sp_rich_area, color=taxon), show.legend = FALSE)+
  geom_smooth(method=lm, aes(x=SVI, y=sp_rich_area, color=taxon))+
 # scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_color_manual(values=parkcolors)+
  ylab("Species Richness / area") +
 # scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  xlab("Social Vulnerability Index")+
  #  ggtitle("All taxa") +
  facet_grid(~taxon, scales = "free_y") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#ggsave(filename="rich_svi_trend.jpg", width = 5, height = 4, units = c("in"), dpi = 300, 
#       path = "~/Documents/Projects/LuxuryNYC/Figures")



