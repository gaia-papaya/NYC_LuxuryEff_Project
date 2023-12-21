
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
setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Buffers/Differences_750m")
#setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2")

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

#### _Social Vulnerabilty Index  ####

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Rasters/SVI-2018-nad83-geotiff/svi_2018_tract_overall_nad83_nopop.tif")

#this is the distance you want to buffer
buff_dist <- 500 

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
  clipped_raster <- raster::crop(ras, extent(shp))  #this line is for uploading ALREADY BUFFERED shapefiles
  # clipped_raster <- raster::crop(ras, extent(buffered_shape))

  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "SVI"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
 # df$SVI_mean <- mean(df$SVI)
  
  # Save the updated shapefile
#  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
SVI_df <- data.table::rbindlist(outlist)


#### _Income ####

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Rasters/SVI-2018-nad83-geotiff/svi_2018_tract_socioeconomic_nad83_nopop.tif")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))
  #clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "SocioEco"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
#  df$SVI_mean <- mean(df$SocioEco)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
SocioEco_df <- data.table::rbindlist(outlist)



#### _Minority Status  #####

#### _Income ####

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Rasters/SVI-2018-nad83-geotiff/svi_2018_tract_minority_nad83_nopop.tif")

#this is the distance you want to buffer
buff_dist <- 500 

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))
  #clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "Minority"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of value to df
  #  df$SVI_mean <- mean(df$SocioEco)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}


#bind all data frames together
Minority_df <- data.table::rbindlist(outlist)

#######################.
#### LINK ALL DATA ####
#######################.
cencus_data <- SVI_df
cencus_data <- left_join(cencus_data, SocioEco_df,  by = c("x", "y", "park"))
cencus_data <- left_join(cencus_data, Minority_df,  by = c("x", "y", "park"))

write_csv(cencus_data, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")

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
  

# SVI
colors<-c(morningside, inwood, crotona, highbridge, vancortlandt, soundview, pelham)

ggplot(data=SVI_df, aes(x=reorder(park, SVI, FUN=mean), y=SVI, fill=park)) + 
  geom_point()+
#  geom_violin(aes(x=park, y=SVI))+
  geom_boxplot()+
  scale_fill_manual(values=colors)+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab(" ") +
  xlab("")+
  ggtitle("2018 Social Vulnerability Index") +
  theme_classic()+
  theme(
    plot.title = element_text(hjust=0.5)
  )
ggsave(file="~/Documents/GIS/NY/Images/SVI2018.jpg", width=5, height=7, units="in")

ggplot(data=SVI_df, aes(x=reorder(park, SVI, FUN=mean), y=SVI)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
    geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2018 Social Vulnerability Index") +
  theme_classic()

# Socioeconomic status
  #Below 150% Poverty
  #Unemployed
  #Housing Cost Burden
  #No High School Diploma
  #No Health Insurance

colors<-c(morningside, inwood, crotona, highbridge, vancortlandt, soundview, pelham)
ggplot(data=SocioEco_df, aes(x=reorder(park, SocioEco, FUN=mean), y=SocioEco, fill=park)) + 
 # geom_point()+
  geom_violin(show.legend=FALSE)+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black", show.legend=FALSE) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_manual(values=colors)+
  xlab("")+
  xlab("") +
  ggtitle("Socioeconomic status") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust=0.5)
  )
ggsave(file="~/Documents/GIS/NY/Images/SocioEconomics2018.jpg", width=3, height=4, units="in")

# Minority status
  #Racial & Ethnic Minority Status
  #Hispanic or Latino (of any race); Black and African American, Not Hispanic or Latino; American Indian and Alaska Native, Not Hispanic or Latino; Asian, Not Hispanic or Latino; Native Hawaiian and Other Pacific Islander, Not Hispanic or Latino; Two or More Races, Not Hispanic or Latino; Other Races, Not Hispanic or Latino
  
ggplot(data=Minority_df, aes(x=reorder(park, Minority, FUN=mean), y=Minority)) + 
  geom_point()+
  geom_violin()+
  stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  xlab("2018 Minority & Language") +
  theme_classic()




