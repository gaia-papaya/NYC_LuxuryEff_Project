#This is the script to read and process pollinator survey data
#Author: Gaia
#dependencies
require(tidyverse)
require(data.table)

#read in biodviersty data csv's
Pollinator_Data_RAW <- as.data.table(read.csv("Rdata/Biodiversity/PollinatorSurvey_2023-12-14.csv"))
Bird_Data_RAW <- as.data.table(read.csv("Rdata/Biodiversity/BirdSurveys_2023-12-14.csv"))

#calculate time spent sampling (in minutes)
Pollinator_Data_RAW[, SamplingTime := difftime(as.POSIXct(TimeEnd, format = "%H:%M"),as.POSIXct(TimeStart, format = "%H:%M"), units =  "mins")]
Bird_Data_RAW[, SamplingTime := difftime(as.POSIXct(TimeEnd, format = "%H:%M"),as.POSIXct(TimeStart, format = "%H:%M"), units =  "mins")]

#filter out fly over observations from bird data
Bird_Data_filtered <- Bird_Data_RAW[!str_which(Distance, "[:alpha:]")]

#clean up species names
Bird_Data_filtered[,Species := str_to_lower(Species)]
Bird_Data_filtered[Species == "titmouse", Species := "tuftedtitmouse"]
sort(unique(Bird_Data_filtered$Species))

sort(unique(Pollinator_Data_RAW$Species))

#calculate richness
Bird_Data_filtered[, Richness := length(unique(Species)), by = list(Site, Date)]


#pivot tables to calculate diversity indices
Bird_Data_wide <- Bird_Data_filtered  %>% 
  pivot_wider(names_from = "Species", values_from = "SpCount", values_fn = sum, values_fill = 0)
