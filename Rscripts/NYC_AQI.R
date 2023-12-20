
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# This code loops through a folder with shape files (of NYC parks), 
# creates a buffer around each, then clips to a raster
# Rasters used here are from CDC Social Vulnerability Index (2018) aligned to U.S. Census grids

# Created Dec 2023 by Valentina Alaasam

####################################.
####  Import park shape files   ####
####################################.

#Set wd to whatever folder contains the folder that contains the shape files
#setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Buffers/Differences/EPSG_5070")
setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Buffers/ParkShapeFiles_Raf2")

#Get the shapefile names from the shapefile folder
shapefile_list <- list.files(pattern="\\.shp")  #folder with all files is named "Files"

#get name from filenames (originals)
shape_file_names <- data.frame(file = shapefile_list)
shape_file_names <- shape_file_names %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","drop1"), sep = ".shp")


################################.
####  Clip Various Rasters  ####
################################.

#### _Particulate Matter (2.5) 2019  ####

#import the raster
ras <- raster("~/Documents/GIS/NY/Rasters/AQI/AnnAvg1_13_300mRaster/aa12_pm300m/prj.adf")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
 # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  #clipped_raster <- raster::crop(ras, extent(shp))
  clipped_raster <- raster::crop(ras, extent(buffered_shape))

  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "PM2.5"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
 # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
#  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
PM_df <- data.table::rbindlist(outlist)



#### _Black Carbon 2019  ####

#import the raster
ras <- raster("~/Documents/GIS/NY/Rasters/AQI/AnnAvg1_13_300mRaster/aa12_bc300m/prj.adf")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  #clipped_raster <- raster::crop(ras, extent(shp))
  clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "BC"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
BC_df <- data.table::rbindlist(outlist)



#### _Nitrous Oxide (NO) 2019  ####

#import the raster
ras <- raster("~/Documents/GIS/NY/Rasters/AQI/AnnAvg1_13_300mRaster/aa12_no300m/prj.adf")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  #clipped_raster <- raster::crop(ras, extent(shp))
  clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "NO"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
NO_df <- data.table::rbindlist(outlist)




#### _Nitrogen Dioxide (NO2) 2019  ####

#import the raster
ras <- raster("~/Documents/GIS/NY/Rasters/AQI/AnnAvg1_13_300mRaster/aa12_no2300m/prj.adf")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  #clipped_raster <- raster::crop(ras, extent(shp))
  clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "NO2"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
NO2_df <- data.table::rbindlist(outlist)






#### _Ozone (O3) 2019  ####

#import the raster
ras <- raster("~/Documents/GIS/NY/Rasters/AQI/AnnAvg1_13_300mRaster/s12_o3300m/prj.adf")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  #clipped_raster <- raster::crop(ras, extent(shp))
  clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "O3"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
O3_df <- data.table::rbindlist(outlist)



#######################.
#### LINK ALL DATA ####
#######################.
AQI_data <- PM_df
AQI_data <- left_join(AQI_data, BC_df,  by = c("x", "y", "park"))
AQI_data <- left_join(AQI_data, NO_df,  by = c("x", "y", "park"))
AQI_data <- left_join(AQI_data, NO2_df,  by = c("x", "y", "park"))
AQI_data <- left_join(AQI_data, O3_df,  by = c("x", "y", "park"))


###############.
#### Plots ####
###############.

# BC
ggplot(data=AQI_data, aes(x=reorder(park, BC, FUN=mean), y=BC)) + 
  geom_point()+
  geom_violin(aes(x=park, y=BC))+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  #geom_boxplot()+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2019 Black Carbon") +
  theme_classic()

# ppm
ggplot(data=AQI_data, aes(x=reorder(park, PM2.5, FUN=mean), y=PM2.5)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2019 PPM 2.5") +
  theme_classic()

# NO
ggplot(data=AQI_data, aes(x=reorder(park, NO, FUN=mean), y=NO)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2019 Nitrous Oxide") +
  theme_classic()


# NO2
ggplot(data=AQI_data, aes(x=reorder(park, NO2, FUN=mean), y=NO2)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2019 Nitrogen Dioxide") +
  theme_classic()


# O3
ggplot(data=O3_df, aes(x=reorder(park, O3, FUN=mean), y=O3)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2019 Ozone") +
  theme_classic()
