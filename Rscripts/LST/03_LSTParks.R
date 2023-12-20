
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# This code loops through a folder with shape files (of NYC parks), 
# creates a buffer around each, then clips to a raster
# clips to from raster of LST from LandSat 8

# Created Dec 2023 by Valentina Alaasam

####################################.
####  Import park shape files   ####
####################################.

#Set wd to whatever folder contains the folder that contains the shape files
setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Buffers/Differences_750m")
#setwd("~/Documents/GIS/NY/ParkShapeFiles_Raf2")

#Get the shapefile names from the shapefile folder
shapefile_list <- list.files(pattern="\\.shp")  #folder with all files is named "Files"

#get name from filenames (differences)
shape_file_names <- data.frame(file = shapefile_list)
shape_file_names <- shape_file_names %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","drop1", "drop2"), sep = "_")

#get name from filenames (originals)
#shape_file_names <- data.frame(file = shapefile_list)
#shape_file_names <- shape_file_names %>% 
#  mutate(file=file) %>% 
#  separate(file, into = c("park","drop1"), sep = ".shp")


################################.
####  Clip Various Rasters  ####
################################.

####Land Surface Temperature

# _MEAN SUMMER DEVIATION

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/LST/mean_summer_deviation_f.tif")

#this is the distance you want to buffer
#buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
 # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))  #this line is for uploading ALREADY BUFFERED CUT-OUTS OF shapefiles
  # clipped_raster <- raster::crop(ras, extent(buffered_shape)) # this line is for using ORIGINAL SHAPE-FILES

  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "MeanSummerDevF"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
 # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
#  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
MeanSummerDevF_df <- data.table::rbindlist(outlist)


# _MAX SUMMER DEVIATION

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/LST/max_summer_deviation_f.tif")

#this is the distance you want to buffer
#buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))  #this line is for uploading ALREADY BUFFERED CUT-OUTS OF shapefiles
  # clipped_raster <- raster::crop(ras, extent(buffered_shape)) # this line is for using ORIGINAL SHAPE-FILES
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "MaxSummerDevF"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
MaxSummerDevF_df <- data.table::rbindlist(outlist)



# _MEAN SUMMER RAW

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/LST/mean_summer_f.tif")

#this is the distance you want to buffer
#buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))  #this line is for uploading ALREADY BUFFERED CUT-OUTS OF shapefiles
  # clipped_raster <- raster::crop(ras, extent(buffered_shape)) # this line is for using ORIGINAL SHAPE-FILES
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "MeanSummerF"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
MeanSummerF_df <- data.table::rbindlist(outlist)


# _MAX SUMMER 

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/LST/max_summer_f.tif")

#this is the distance you want to buffer
#buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))  #this line is for uploading ALREADY BUFFERED CUT-OUTS OF shapefiles
  # clipped_raster <- raster::crop(ras, extent(buffered_shape)) # this line is for using ORIGINAL SHAPE-FILES
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "MaxSummerF"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
MaxSummerF_df <- data.table::rbindlist(outlist)


#######################.
#### LINK ALL DATA ####
#######################.
LST_df <- MeanSummerDevF_df
LST_df <- left_join(LST_df, MaxSummerDevF_df,  by = c("x", "y", "park"))
LST_df <- left_join(LST_df, MeanSummerF_df,  by = c("x", "y", "park"))
LST_df <- left_join(LST_df, MaxSummerF_df,  by = c("x", "y", "park"))

write_csv(LST_df, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/LST/LST_Parks.csv")











###############.
#### Plots ####
###############.
morningside= "#8d5a99"
inwood="#a47158"
vancortlandt="#e8718d"
soundview="#097d79"
pelham="#e5b636"
highbridge="#e77148"
crotona="#54a82d"
  

# max summer temp - deviation
colors<-c(morningside, inwood, crotona, highbridge, vancortlandt, soundview, pelham)

ggplot(data=LST_df, aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park)) + 
#  geom_point()+
  geom_violin(aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park))+
#  geom_boxplot(aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park))+
  scale_fill_manual(values=colors)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab(" ") +
  xlab("")+
  ggtitle("Max Summer Temps (F) - deviation from mean") +
  theme_classic()+
  theme(
    plot.title = element_text(hjust=0.5)
  )
#ggsave(file="~/Documents/GIS/NY/Images/SVI2018.jpg", width=5, height=7, units="in")



# mean summer temp - deviation
colors<-c(morningside, inwood, crotona, highbridge, vancortlandt, soundview, pelham)

ggplot(data=LST_df, aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park)) + 
  #  geom_point()+
  geom_violin(aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park))+
  #  geom_boxplot(aes(x=reorder(park, MaxSummerDevF, FUN=mean), y=MaxSummerDevF, fill=park))+
  scale_fill_manual(values=colors)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab(" ") +
  xlab("")+
  ggtitle("Max Summer Temps (F) - deviation from mean") +
  theme_classic()+
  theme(
    plot.title = element_text(hjust=0.5)
  )