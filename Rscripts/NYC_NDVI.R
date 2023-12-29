
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)

# This code loops through a folder with shape files (of NYC parks), 
# creates a buffer around each, then clips to a raster
# Rasters used here are from Planet Quarterly 2022-2023 April-July or July-Oct

# Created Dec 2023 by Valentina Alaasam


### ndvi inside parks ####

####################################.
####  _Import park shape files   ####
####################################.

#Set wd to whatever folder contains the folder that contains the shape files
#setwd("~/Documents/GIS/NY/Buffers/Differences/EPSG_5070")
setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2")

#Get the shapefile names from the shapefile folder
shapefile_list <- list.files(pattern="\\.shp")  #folder with all files is named "Files"

#get name from filenames (differemces)
#shape_file_names <- data.frame(file = shapefile_list)
#shape_file_names <- shape_file_names %>% 
#  mutate(file=file) %>% 
#  separate(file, into = c("park","drop1", "drop2"), sep = "_")
#parks<-shape_file_names

#get name from filenames (originals)
shape_file_names <- data.frame(file = shapefile_list)
shape_file_names <- shape_file_names %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","drop1"), sep = ".shp")


################################.
####  _Clip Various Rasters  ####
################################.

#### ndvi  INSIDE PARK ####

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Rasters/NDVI_Planet_quarterly/NDVI_Quartwerly_2022-23_April-July.tif")

#this is the distance you want to buffer (no buffer for ndvi, interested in within park greenness)
#buff_dist <- 500 

# hard-coding # of bins based on full dataset
bins<-c(seq(0,1,.05))
labels<-c(seq(0.05,1,.05))

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
 # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
 # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))
  #clipped_raster <- raster::crop(ras, extent(buffered_shape))

  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "NDVI"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of ndvi for plot ordering
  df$mean_ndvi<-mean(df$NDVI)
  
  #add 20 ndvi bins
  df1<- df %>% mutate(ndvi_bins=cut(NDVI, breaks = bins, include.lowest =TRUE, labels=labels)) %>% 
  group_by(park, mean_ndvi, ndvi_bins) %>% summarise(n = n())
  
  #add proportion of differeing bins
  sum<-sum(df1$n)
  df1 <- df1 %>% 
    group_by(ndvi_bins) %>% 
    mutate(percent_ndvi_bin = n/sum*100)
  
  # Save the updated shapefile
#  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df1
}
#bind all data frames together
NDVI_df <- data.table::rbindlist(outlist)

#write_csv(NDVI_df, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ndvi_nycparks.csv")



##################.
####  _plots  ####
##################.

NDVI_df<-read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ndvi_nycparks.csv")
#plot
#remove central
NDVI_df<-subset(NDVI_df, !NDVI_df$park =="CentralPark")


#plot with bins
library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(20)

ggplot(data = NDVI_df, aes(y = percent_ndvi_bin,
                        # x=park,
                             x = reorder(park, mean_ndvi),
                         fill=as.factor(ndvi_bins)))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("% pixels within parks")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    axis.text.y=element_blank()
  )

#reorder parks by SVI
NDVI_df<- NDVI_df %>% mutate(park=fct_relevel(park, 
                                                  "Morningside", "InwoodHill", "Crotona", "Highbridge", "VanCortlandt", 
                                                  "Soundview", "PelhamBaySouth" )) 

ggplot(data = NDVI_df, aes(y = percent_ndvi_bin,
                            x=park,
                          # x = reorder(park, mean_ndvi),
                           fill=as.factor(ndvi_bins)))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("% pixels within parks")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    axis.text.y=element_blank()
  )

#ggsave(filename="ndvi_1.jpg", width = 5, height = 3, units = c("in"), dpi = 300, 
 #      path = "~/Documents/Projects/LuxuryNYC/Figures")



#Add SVI
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
cencus_data_summary<-cencus_data %>% group_by(park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))
cencus_data_summary$park
unique(NDVI_df$park)
cencus_data_summary$park[which(cencus_data_summary$park=="Inwood")]<-"InwoodHill"
NDVI_df<-left_join(NDVI_df, cencus_data_summary, by="park")


ggplot(data = NDVI_df, aes( y = mean_ndvi,
                            # x=park,
                             x=SVI,
                             # x = reorder(park, SVI),
                            color=SVI))+
  geom_point(size=4) +
  geom_smooth(method=lm, se=TRUE, aes(y=mean_ndvi, x=SVI), color="black", alpha=0.2) +
  scale_color_gradient(high = "#800000", low = "#EEE8AA")+
  #  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("Mean NDVI (within park)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    # axis.text.y=element_blank()
  )


ggplot(data = NDVI_df, aes( y = mean_ndvi,
                                       #   x=park,
                                       x=SVI,
                                       #  x = reorder(park, SVI),
                                       color=SVI))+
  geom_point(size=4, show.legend = FALSE) +
  geom_smooth(method=lm, se=TRUE, aes(y=mean_ndvi, x=SVI), color="black", alpha=0.2) +
  scale_color_gradient(high = "#800000", low = "#EEE8AA")+
  #  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("Mean NDVI (within park)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    # axis.text.y=element_blank()
  )

ggsave(filename="ndvi_svi.jpg", width = 6, height = 3, units = c("in"), dpi = 300, 
       path = "~/Documents/Projects/LuxuryNYC/Figures")






### ndvi OUTSIDE parks ####

####################################.
####  _Import park shape files   ####
####################################.

#Set wd to whatever folder contains the folder that contains the shape files
#setwd("~/Documents/GIS/NY/Buffers/Differences/EPSG_5070")
setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Buffers/Differences_750m")

#Get the shapefile names from the shapefile folder
shapefile_list <- list.files(pattern="\\.shp")  #folder with all files is named "Files"

#get name from filenames (differemces)
shape_file_names <- data.frame(file = shapefile_list)
shape_file_names <- shape_file_names %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","drop1", "drop2"), sep = "_")
parks<-shape_file_names

#get name from filenames (originals)
#shape_file_names <- data.frame(file = shapefile_list)
#shape_file_names <- shape_file_names %>% 
#  mutate(file=file) %>% 
#  separate(file, into = c("park","drop1"), sep = ".shp")


################################.
####  _Clip Various Rasters  ####
################################.

#### ndvi  INSIDE PARK ####

#import the raster
ras <- raster("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/Rasters/NDVI_Planet_quarterly/NDVI_Quartwerly_2022-23_April-July.tif")

#this is the distance you want to buffer (no buffer for ndvi, interested in within park greenness)
#buff_dist <- 500 

# hard-coding # of bins based on full dataset
bins<-c(seq(0,1,.05))
labels<-c(seq(0.05,1,.05))

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))
  #clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "NDVI"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of ndvi for plot ordering
  df$mean_ndvi<-mean(df$NDVI)
  
  #add 20 ndvi bins
  df1<- df %>% mutate(ndvi_bins=cut(NDVI, breaks = bins, include.lowest =TRUE, labels=labels)) %>% 
    group_by(park, mean_ndvi, ndvi_bins) %>% summarise(n = n())
  
  #add proportion of differeing bins
  sum<-sum(df1$n)
  df1 <- df1 %>% 
    group_by(ndvi_bins) %>% 
    mutate(percent_ndvi_bin = n/sum*100)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df1
}
#bind all data frames together
NDVI_750mbuffer_df <- data.table::rbindlist(outlist)

#write_csv(NDVI_750mbuffer_df, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ndvi_NDVI_750mbuffer.csv")



##################.
####  _plots  ####
##################.

NDVI_750mbuffer_df<-read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ndvi_NDVI_750mbuffer.csv")
#plot

#remove central park
NDVI_750mbuffer_df<-subset(NDVI_750mbuffer_df, !NDVI_750mbuffer_df$park =="CentralPark")

#plot with bins
library(RColorBrewer)
mycolors <- colorRampPalette(brewer.pal(9, "Greens"))(20)

ggplot(data = NDVI_750mbuffer_df, aes(y = percent_ndvi_bin,
                           # x=park,
                           x = reorder(park, mean_ndvi),
                           fill=as.factor(ndvi_bins)))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("% pixels in surrounding 750m buffer")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    axis.text.y=element_blank()
  )

#reorder parks by SVI
NDVI_750mbuffer_df<- NDVI_750mbuffer_df %>% mutate(park=fct_relevel(park, 
                                              "Morningside", "InwoodHill", "Crotona", "Highbridge", "VanCortlandt", 
                                              "Soundview", "PelhamBaySouth" )) 

ggplot(data = NDVI_750mbuffer_df, aes(y = percent_ndvi_bin,
                           x=park,
                           # x = reorder(park, mean_ndvi),
                           fill=as.factor(ndvi_bins)))+
  geom_bar(stat = "identity") +
  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("% pixels in surrounding 750m buffer")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    axis.text.y=element_blank()
  )


#Add SVI
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
cencus_data_summary<-cencus_data %>% group_by(park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))
cencus_data_summary$park[which(cencus_data_summary$park=="Inwood")]<-"InwoodHill"
NDVI_750mbuffer_df$park[which(NDVI_750mbuffer_df$park=="Inwood")]<-"InwoodHill"
cencus_data_summary$park
unique(NDVI_750mbuffer_df$park)
NDVI_750mbuffer_df<-left_join(NDVI_750mbuffer_df, cencus_data_summary, by="park")



ggplot(data = NDVI_750mbuffer_df, aes( y = mean_ndvi,
                                       #   x=park,
                                       #x=SVI,
                                         x = reorder(park, SVI),
                                       color=SVI))+
  geom_point(size=4) +
  geom_smooth(method=lm, se=TRUE, aes(y=mean_ndvi, x=SVI), color="black", alpha=0.2) +
  scale_color_gradient(high = "#800000", low = "#EEE8AA")+
  #  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("Mean NDVI (750m surroundings)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    # axis.text.y=element_blank()
  )


ggplot(data = NDVI_750mbuffer_df, aes( y = mean_ndvi,
                                       #   x=park,
                                       x=SVI,
                                      #  x = reorder(park, SVI),
                                       color=SVI))+
  geom_point(size=4) +
  geom_smooth(method=lm, se=TRUE, aes(y=mean_ndvi, x=SVI), color="black", alpha=0.2) +
  scale_color_gradient(high = "#800000", low = "#EEE8AA")+
  #  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("Mean NDVI (750m surroundings)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    # axis.text.y=element_blank()
  )

#ggsave(filename="ndvi_svi.jpg", width = 5, height = 3, units = c("in"), dpi = 300, 
#       path = "~/Documents/Projects/LuxuryNYC/Figures")


ggplot(data = NDVI_750mbuffer_df, aes( y = mean_ndvi,
                                       #   x=park,
                                       x=SVI,
                                       #  x = reorder(park, SVI),
                                       color=SVI))+
  geom_point(size=4) +
  geom_smooth(method=lm, se=TRUE, aes(y=mean_ndvi, x=SVI), color="black", alpha=0.2) +
  scale_color_gradient(high = "#800000", low = "#EEE8AA")+
  #  scale_fill_manual(values = mycolors) +
  # scale_fill_brewer(palette = "Greens")+
  # scale_fill_paletteer_d(palleteer_dynamic("cartography::green.pal", 24, dynamic=FALSE, direction=-1))+	
  #  scale_fill_paletteer_d(palleteer_dynamic("grDevices::Greens 3", 24, dynamic=FALSE, direction=-1))+	
  guides(fill = guide_legend(title = "ndvi"))+
  ylab("Mean NDVI (750m surroundings)")+
  theme_classic()+
  theme(
    axis.text.x = element_text(angle = 40, hjust = 1, size = 10),
    axis.title.x=element_blank(),
    # axis.text.y=element_blank()
  )




#######################.
#### ANALYSIS ####
##################.

#looping with no bins

#this loop takes each shapefile, buffers it, clips the raster, and them summarizes the data
outlist <- list()
for (i in 1:length(shapefile_list)) {
  
  # Read the shapefile
  shp <- st_read(shapefile_list[i])
  
  #Changes the projection to match the raster
  # shp <- spTransform(shp, crs(ras))
  shp <- st_transform(shp, crs = raster::crs(ras))
  
  #create buffer around shape
  # buffered_shape <- st_buffer(shp, dist = buff_dist)
  
  # Clip the raster to the buffered shape
  clipped_raster <- raster::crop(ras, extent(shp))
  #clipped_raster <- raster::crop(ras, extent(buffered_shape))
  
  #masked_raster <- raster::mask(clipped_raster, shp) #gives all values outside the circle an NA value
  
  #This makes the raster into a dataframe
  #dat <- raster::as.data.frame(masked_raster, na.rm=T)
  df <- raster::as.data.frame(clipped_raster, xy=TRUE, na.rm=T)
  colnames(df)[3] <- "NDVI"
  
  #add park name to df
  df$park <- shape_file_names$park[i]
  
  #add mean of ndvi for plot ordering
  df$mean_ndvi<-mean(df$NDVI)
  
  #add 20 ndvi bins
 # df1<- df %>% mutate(ndvi_bins=cut(NDVI, breaks = bins, include.lowest =TRUE, labels=labels)) %>% 
 #   group_by(park, mean_ndvi, ndvi_bins) %>% summarise(n = n())
  
  #add proportion of differeing bins
 # sum<-sum(df1$n)
#  df1 <- df1 %>% 
#    group_by(ndvi_bins) %>% 
#    mutate(percent_ndvi_bin = n/sum*100)
  
  # Save the updated shapefile
  #  st_write(shp, paste0("output_", shapefile))
  outlist[[i]] <- df
}
#bind all data frames together
NDVI_raw_df <- data.table::rbindlist(outlist)


#Add SVI
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
cencus_data_summary<-cencus_data %>% group_by(park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))
cencus_data_summary$park[which(cencus_data_summary$park=="Inwood")]<-"InwoodHill"
NDVI_raw_df$park[which(NDVI_raw_df$park=="Inwood")]<-"InwoodHill"
cencus_data_summary$park
unique(NDVI_raw_df$park)
NDVI_raw_df<-left_join(NDVI_raw_df, cencus_data_summary, by="park")


model1<-glm(NDVI ~ SVI , data=NDVI_raw_df, family=poisson)
summary(model1)
