
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(dplyr)
library(stringdist)


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
nrow(nyctrees)
length(unique(nyctrees$PlantingSpaceGlobalID))

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
  select(scientific, common, W, N)

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



##### search for typos #####
# Calculate string distances and find similar strings
similar_strings <- stringdistmatrix(parktrees$scientific, parktrees$scientific, method = "jaccard") <= 2

# Display similar strings
parktrees[similar_strings, ]


# Use the amatch function to find approximate matches
matches <- amatch(parktrees$scientific, parktrees$scientific, method = "lv")  # "lv" stands for Levenshtein distance

# Identify rows with potential typos
potential_typos <- matches > 0 & matches <= threshold

# Display rows with potential typos
df[potential_typos, ]