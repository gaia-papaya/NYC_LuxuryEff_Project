setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/")


# Load dependencies to start
source("Rscripts/LST/00_load_dependencies.R")
library(terra)


################################################################################
# load data
################################################################################
nyc = st_read("Rdata/LandSat8_LST_30m_2020_2023/NY_shapefile/State.shp") %>%
  st_transform("+proj=longlat +datum=WGS84")
                

#original NY shape file does not extend north enough
#nyc = st_read("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>%
#  st_transform("+proj=longlat +datum=WGS84")

all_mean_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_surfacetemperature_mean_2020_2023.tif')
all_mean_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_surfacetemperature_mean_2020_2023.tif')
all_min_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_surfacetemperature_min_2020_2023.tif')
all_min_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_surfacetemperature_min_2020_2023.tif')
all_max_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_surfacetemperature_max_2020_2023.tif')
all_max_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_surfacetemperature_max_2020_2023.tif')
all_median_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_surfacetemperature_median_2020_2023.tif')
all_median_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_surfacetemperature_median_2020_2023.tif')

W_mean_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_W_surfacetemperature_mean_2020_2023.tif')
W_mean_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_W_surfacetemperature_mean_2020_2023.tif')
W_min_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_W_surfacetemperature_min_2020_2023.tif')
W_min_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_W_surfacetemperature_min_2020_2023.tif')
W_max_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_W_surfacetemperature_max_2020_2023.tif')
W_max_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_W_surfacetemperature_max_2020_2023.tif')
W_median_summer = raster('Rdata/LandSat8_LST_30m_2020_2023/summer_W_surfacetemperature_median_2020_2023.tif')
W_median_winter = raster('Rdata/LandSat8_LST_30m_2020_2023/winter_W_surfacetemperature_median_2020_2023.tif')


################################################################################
# Merge Westchester with main rasters
################################################################################

mean_summer <- raster::merge(all_mean_summer, W_mean_summer)
mean_winter <- raster::merge(all_mean_winter, W_mean_winter)
min_summer <- raster::merge(all_min_summer, W_min_summer)
min_winter <- raster::merge(all_min_winter, W_min_winter)
max_summer <- raster::merge(all_max_summer, W_max_summer)
max_winter <- raster::merge(all_max_winter, W_max_winter)
median_summer <- raster::merge(all_median_summer, W_median_summer)
median_winter <- raster::merge(all_median_winter, W_median_winter)


################################################################################
# prep temp raster 
################################################################################

nyc = st_transform(nyc, projection(mean_summer))

###Mean Summer
mean_summer = mean_summer %>% crop(nyc) %>% mask(nyc)
mean_summer = mean_summer*0.00341802 + 149 #the scale and offset parameters on GEE
mean_summer = k_to_f(mean_summer)
writeRaster(mean_summer, "Rdata/output/LST/mean_summer_f", 
            format = "GTiff", overwrite = T)

mean_summer_deviation = mean_summer - mean(values(mean_summer), na.rm=T)

# save out the deviation
writeRaster(mean_summer_deviation, "Rdata/output/LST/mean_summer_deviation_f", 
            format = "GTiff", overwrite = T)

###Max Summer
max_summer = max_summer %>% crop(nyc) %>% mask(nyc)
max_summer = max_summer*0.00341802 + 149 #the scale and offset parameters on GEE
max_summer = k_to_f(max_summer)
writeRaster(max_summer, "Rdata/output/LST/max_summer_f", 
            format = "GTiff", overwrite = T)

max_summer_deviation = max_summer - mean(values(max_summer), na.rm=T)

# save out the deviation
writeRaster(max_summer_deviation, "Rdata/output/LST/max_summer_deviation_f", 
            format = "GTiff", overwrite = T)



###Mean Winter
mean_winter = mean_summer %>% crop(nyc) %>% mask(nyc)
mean_winter = mean_summer*0.00341802 + 149 #the scale and offset parameters on GEE
mean_winter = k_to_f(mean_summer)
writeRaster(mean_winter, "Rdata/output/LST/mean_winter_f", 
            format = "GTiff", overwrite = T)

mean_winter_deviation = mean_winter - mean(values(mean_winter), na.rm=T)

# save out the deviation
writeRaster(mean_winter_deviation, "Rdata/output/LST/mean_winter_deviation_f", 
            format = "GTiff", overwrite = T)

###Max Winter
max_winter = max_winter %>% crop(nyc) %>% mask(nyc)
max_winter = max_winter*0.00341802 + 149 #the scale and offset parameters on GEE
max_winter = k_to_f(max_summer)
writeRaster(max_winter, "Rdata/output/LST/max_winter_f", 
            format = "GTiff", overwrite = T)

max_winter_deviation = max_winter - mean(values(max_winter), na.rm=T)

# save out the deviation
writeRaster(max_winter_deviation, "Rdata/output/LST/max_winter_deviation_f", 
            format = "GTiff", overwrite = T)



################################################################################
# create plot 
################################################################################

# capping the outliers ---------------------------------------------------------
# since leaflet coloring is linear, this lets us use the larger range

deviation_plot = mean_winter_deviation
#deviation_plot = mean_summer_deviation

values(deviation_plot) = ifelse(values(deviation_plot) <= -8, -8, values(deviation_plot))
values(deviation_plot) = ifelse(values(deviation_plot) >= 8, 8, values(deviation_plot))

#need to crop raster, too big as-is to run leaflet
deviation_plot <- crop(deviation_plot, extent(-74.1, -73.45, 40.6, 41.2))


# mapping 
library(mapview)
heat_pal = colorNumeric(colorRamps::matlab.like(15),
                        domain = c(values(deviation_plot), 
                                   # extend domain past so border values aren't NA
                                   min(values(deviation_plot), na.rm=T)-0.1, 
                                   max(values(deviation_plot), na.rm=T)+0.1),
                        na.color = "transparent")

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 17)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 10, maxZoom = 17)) %>%
  addRasterImage(deviation_plot, colors = heat_pal, opacity = 0.4) %>% 
  addLegend_decreasing(position = "topleft", 
            pal = heat_pal, 
            values = values(deviation_plot), 
            title = paste0("Temperature Deviation", "<br>", "from Mean"),  
            labels = c("> 8°", "6°", "4°", "2°", "0°", "-2°", 
                       "-4°", "-6°", "< -8°"), 
            decreasing = T)


#saveWidget(map, file = file.path("Rdata/output/LST/visuals", "mean_winter_heat_deviation_raster.html"))
#mapshot(map, file = file.path("Rdata/output/LST/visuals", "mean_winter_heat_deviation_raster.png"),
#         remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)

saveWidget(map, file = file.path("Rdata/output/LST/visuals", "mean_summer_heat_deviation_raster.html"))
mapshot(map, file = file.path("Rdata/output/LST/visuals", "mean_summer_heat_deviation_raster.png"),
        remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)


################################################################################
# smoothed plot - 1.5 block average
################################################################################
#SUMMER MEAN

n = 27
deviation_smooth = focal(mean_summer_deviation, w = matrix(rep(1, n^2), nrow = n), 
                         fun = "mean", na.rm = T, pad = T) %>% mask(nyc)

values(deviation_smooth) = ifelse(values(deviation_smooth) <= -8, -8, values(deviation_smooth))
values(deviation_smooth) = ifelse(values(deviation_smooth) >= 8, 8, values(deviation_smooth))

heat_pal = colorNumeric(colorRamps::matlab.like(15),
                        domain = c(values(deviation_smooth), 
                                   # extend domain past so border values aren't NA
                                   min(values(deviation_smooth), na.rm=T)-0.1, 
                                   max(values(deviation_smooth), na.rm=T)+0.1),
                        na.color = "transparent")

writeRaster(deviation_smooth, "Rdata/output/LST/mean_summer_deviation_smooth_f", 
            format = "GTiff", overwrite = T)

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
  addRasterImage(deviation_smooth, colors = heat_pal, opacity = 0.4) %>% 
  addLegend_decreasing(position = "topleft", 
                       pal = heat_pal, 
                       values = values(deviation_smooth), 
                       title = paste0("Temperature Deviation", "<br>", "from Mean"),  
                       labels = c("> 8°", "6°", "4°", "2°", "0°", "-2°", 
                                  "-4°", "-6°", "< -8°"), 
                       decreasing = T)

saveWidget(map, file=file.path('Rdata/output/LST/visuals', 
                               "mean_summer_heat_smoothed_deviation_raster.html"))
mapshot(map, 
        file = file.path("Rdata/output/LST/visuals", "mean_summer_heat_smoothed_deviation_raster.png"),
        remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)



#WINTER MEAN

# 9 pixels is about 1 long block, so 27 is about 1.5 blocks in each direction

n = 27
deviation_smooth = focal(mean_winter_deviation, w = matrix(rep(1, n^2), nrow = n), 
                         fun = "mean", na.rm = T, pad = T) %>% mask(nyc)

values(deviation_smooth) = ifelse(values(deviation_smooth) <= -8, -8, values(deviation_smooth))
values(deviation_smooth) = ifelse(values(deviation_smooth) >= 8, 8, values(deviation_smooth))

heat_pal = colorNumeric(colorRamps::matlab.like(15),
                        domain = c(values(deviation_smooth), 
                                   # extend domain past so border values aren't NA
                                   min(values(deviation_smooth), na.rm=T)-0.1, 
                                   max(values(deviation_smooth), na.rm=T)+0.1),
                        na.color = "transparent")

writeRaster(deviation_smooth, "Rdata/output/LST/mean_winter_deviation_smooth_f", 
            format = "GTiff", overwrite = T)

map = leaflet(options = leafletOptions(zoomControl = FALSE, 
                                       minZoom = 10, 
                                       maxZoom = 16)) %>%
  addProviderTiles('CartoDB.Positron', 
                   options = providerTileOptions(minZoom = 10, maxZoom = 16)) %>%
  addRasterImage(deviation_smooth, colors = heat_pal, opacity = 0.4) %>% 
   addLegend_decreasing(position = "topleft", 
                       pal = heat_pal, 
                       values = values(deviation_smooth), 
                       title = paste0("Temperature Deviation", "<br>", "from Mean"),  
                       labels = c("> 8°", "6°", "4°", "2°", "0°", "-2°", 
                                  "-4°", "-6°", "< -8°"), 
                       decreasing = T)


saveWidget(map, file=file.path('Rdata/output/LST/visuals', 
                               "mean_winter_heat_smoothed_deviation_raster.html"))
mapshot(map, 
        file = file.path("Rdata/output/LST/visuals", "mean_winter_heat_smoothed_deviation_raster.png"),
        remove_controls = c("homeButton", "layersControl"), vwidth = 1000, vheight = 850)
          

