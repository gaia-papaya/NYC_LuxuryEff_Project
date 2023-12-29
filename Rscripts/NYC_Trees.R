
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(dplyr)
library(stringdist)
library(vegan)
library(hillR)

#Dataset updated in 2022, downloaded from this site:
#https://scout.tsdataclinic.com/explore/NYC/dataset/k5ta-2trh/joins
# User guide: https://docs.google.com/document/d/1PVPWFi-WExkG3rvnagQDoBbqfsGzxCKNmR6n678nUeU/edit?usp=sharing

nyctrees<-read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/Forestry_Tree_Points.csv")


#modify format
# Location: POINT (-74.15171663027641 40.53411487961452)
# GenusSpecies: Cornus florida - flowering dogwood
# CreatedDate: 2015-04-02 04:00:00.0000000


nyctrees <- nyctrees %>% 
  separate(Location, into = c("lose", "W", "N"), sep = " ") %>%
  mutate(W = as.numeric(str_sub(W, start = 2))) %>%
  mutate(N = as.numeric(str_sub(N, end = -2)))

nyctrees <- nyctrees %>% 
  separate(GenusSpecies, into = c("scientific", "common"), sep = "-")

nyctrees <- nyctrees %>% 
  separate(CreatedDate, into = c("year", "month", "day"), sep = "-") %>% 
  separate(day, into = c("day", "time"), sep = " ")

#Remove trees that were replaced. Each planting space has only 1 tree, meaning duplicates represent replacements
#nrow(nyctrees)
#length(unique(nyctrees$PlantingSpaceGlobalID))

nyctrees <- nyctrees %>% 
  arrange(desc(year)) %>%  #sort by year
  distinct(PlantingSpaceGlobalID, .keep_all = TRUE) #remove duplicates, while keeeping all columns





##### Clip to parks #####

# List all shapefiles in the directory
shapefile_list <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2", 
                             pattern = "\\.shp$", full.names = TRUE)

#get name from filenames (originals)
shapefile_names <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2", 
                              pattern = "\\.shp$")
shape_file_names <- data.frame(file = shapefile_names)
shape_file_names <- shape_file_names %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","drop1"), sep = ".shp")

#get crs from park shape files
park_crs <- crs(st_read(shapefile_list[1]))

#convert nyctrees df into a shapefile
nyctrees_simple <- nyctrees %>%
  select(scientific, common, W, N, year)

df_sf <- st_as_sf(nyctrees_simple, coords = c("W", "N"), crs=4326)
df_sf <- st_transform(df_sf, crs = park_crs)



result_list <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  # Use st_join to keep only the points within the polygons
  clipped <- st_intersection(df_sf, shp)
  
  #add park name to df
  clipped$park <- shape_file_names$park[i]
  
  # Append the result to the list
  result_list[[i]] <- clipped
  print(i)
}

# Combine the results into a single sf object
parktrees <- data.table::rbindlist(result_list)

#save combined df
write_csv(parktrees, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/parktrees_2016-2023.csv")


########################.
#### DATA SHORTCUT  ####
########################.



#read in df
parktrees <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/parktrees_2016-2023.csv")
parktrees$park[which(parktrees$park=="Inwood")]<-"InwoodHill"
parktrees<-subset(parktrees, !park=="CentralPark") 


##### search for typos ####

# Use the amatch function to find approximate matches
matches <- amatch(parktrees$scientific, parktrees$scientific, method = "lv")  # "lv" stands for Levenshtein distance

# Identify rows with potential typos
potential_typos <- matches > 0 & matches <= .8

# Display rows with potential typos
parktrees[potential_typos, ]



### Summarise dataset for species richness
richness_df <- parktrees %>%
  group_by(park) %>%
  summarize(unique_tree_sp = length(unique(scientific)))
  

#Add SVI
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
cencus_data_summary<-cencus_data %>% group_by(park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))
cencus_data_summary$park[which(cencus_data_summary$park=="Inwood")]<-"InwoodHill"
richness_df$park
cencus_data_summary$park
combo<-left_join(cencus_data_summary, richness_df, by="park")


#Add size of park from crime dataset
crime <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/park_crime_21_23.csv") 
crime <- crime %>% select(Park, Acres, crime_total) %>%
  mutate(park=Park, crime_per_acre=(crime_total/Acres))
crime$park[which(crime$park=="Inwood")]<-"InwoodHill"

combo<-left_join(combo, crime, by="park") %>%
  mutate(tree_richness_per_acre=(unique_tree_sp/Acres))

ggplot(data=combo, aes(x=reorder(park, SVI), y=unique_tree_sp, fill=SVI)) +
  geom_bar(stat="identity")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  theme_classic()

ggplot(data=combo, aes(x=reorder(park, SVI), y=tree_richness_per_acre, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree Species Richness / area") +
  xlab("") +
  theme_classic()

ggsave(filename="TreeRichness.jpg", width = 5, height = 4, units = c("in"), dpi = 300, 
    path = "~/Documents/Projects/LuxuryNYC/Figures")



########################.
#### DATA ANALYSIS  ####
########################.

species_abundance <- table(parktrees$park, parktrees$scientific)

# Calculate Shannon diversity index
shannon_index <- as.data.frame(diversity(species_abundance, index = "shannon")) 
colnames(shannon_index)<-"shannon"
shannon_index$park <- rownames(shannon_index)
combo<-left_join(combo, shannon_index, by="park") 
combo <- combo %>%
  mutate(shannon_per_acre=(shannon/Acres))

#Calculate tree density
### Summarise dataset for density
density_df <- parktrees %>%
  group_by(park) %>%
  summarize(total_trees = length(scientific))
combo<-left_join(combo, density_df, by="park") %>%
  mutate(tree_density=(total_trees/Acres))

#Correct richness for density (instead of park size)

#shannons diversity per acre X SVI
ggplot(data=combo, aes(x=reorder(park, SVI), y=shannon_per_acre, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree diversity (shannons) / area") +
  xlab("") +
  theme_classic()
ggsave(filename="TreeShannons.jpg", width = 5, height = 3.4, units = c("in"), dpi = 300, 
       path = "~/Documents/Projects/LuxuryNYC/Figures")


#shannons diversity uncorrected X SVI
ggplot(data=combo, aes(x=reorder(park, SVI), y=shannon, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree diversity (shannons)") +
  xlab("") +
  theme_classic()

#density X SVI
ggplot(data=combo, aes(x=reorder(park, SVI), y=tree_density, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree density") +
  xlab("") +
  theme_classic()


#density X SVI
ggplot(data=combo, aes(x=reorder(park, SVI), y=tree_density, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree density") +
  xlab("") +
  theme_classic()
ggsave(filename="TreeDensity.jpg", width = 6, height = 4, units = c("in"), dpi = 300, 
       path = "~/Documents/Projects/LuxuryNYC/Figures")

       