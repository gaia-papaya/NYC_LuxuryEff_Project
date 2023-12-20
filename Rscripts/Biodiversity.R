#This is the script to read and process pollinator survey data
#Author: Gaia
#dependencies
require(tidyverse)
require(data.table)
require(taxize)

#read in biodviersity data csv's
Pollinator_Data_RAW <- as.data.table(read.csv("Rdata/Biodiversity/PollinatorSurvey_2023-12-20.csv"))
Bird_Data_RAW <- as.data.table(read.csv("Rdata/Biodiversity/BirdSurveys_2023-12-14.csv"))

#fix improper column names
colnames(Pollinator_Data_RAW)[11] <- "ObsTaxa"
#clean up taxa characters for easier queries for taxize
Pollinator_Data_RAW[,ObsTaxa := str_trim(str_replace_all(ObsTaxa, "([[:upper:]])", " \\1"))]
Pollinator_Data_RAW[, ObsTaxa := str_remove(ObsTaxa, "sp.")]
Pollinator_Data_RAW[ObsTaxa == "German Yellowjacket", ObsTaxa := str_replace(ObsTaxa, "Yellowjacket", "Wasp")]
Pollinator_Data_RAW[,ObsTaxa := str_to_lower(ObsTaxa)]
#query Encyclopedia of Life for converting common names to sci names, output into list of char vectors
#(note to self: NEVER WORK WITH COMMON NAMES AGAIN)
common_poll_name_list <- comm2sci(Pollinator_Data_RAW$ObsTaxa, db = "eol")
#TODO: deal with common names vs sci names + adding taxon orders

#mutate original observation names to scientific
Pollinator_Data_RAW <- Pollinator_Data_RAW %>% 
  mutate(ObsTaxa_Sci = )

#calculate time spent sampling (in minutes)
Pollinator_Data_RAW[, SamplingTime := difftime(as.POSIXct(TimeEnd, format = "%H:%M"),as.POSIXct(TimeStart, format = "%H:%M"), units =  "mins")]
Bird_Data_RAW[, SamplingTime := difftime(as.POSIXct(TimeEnd, format = "%H:%M"),as.POSIXct(TimeStart, format = "%H:%M"), units =  "mins")]

#filter out fly over observations from bird data
Bird_Data_filtered <- Bird_Data_RAW[!str_which(Distance, "[:alpha:]")]

#clean up species names----
Bird_Data_filtered[,Species := str_to_lower(Species)]
Bird_Data_filtered[Species == "titmouse", Species := "tuftedtitmouse"]
#sort(unique(Bird_Data_filtered$Species))
Pollinator_Data_RAW[ObsTaxa == "SpottedLanternfly", ObsTaxa := "LycormaDelicatula"]
sort(unique(Pollinator_Data_RAW$ObsTaxa))

#assign taxonomic rank to pollinators based on observed taxa
Pollinator_Data_reclass <- Pollinator_Data_RAW %>%  #this method of reclassing to taxon groups is very ugly and veeery post-hoc, next time maybe we should use scientific names only????? idfk
  mutate(Obs_Order = case_when(str_detect(str_to_lower(ObsTaxa), "bee$") == T | ObsTaxa == "GermanYellowjacket" | ObsTaxa == "EasternYellowjacket"  ~ "Hymenoptera",
                               str_detect(str_to_lower(ObsTaxa), "fly") == T | ObsTaxa == "Delphinia sp." | ObsTaxa == "Syrphini" |
                                 ObsTaxa == "EasternCaligrapher" | ObsTaxa == "EasternCaligrapher" | ObsTaxa == "EasternCaligrapher" |
                                 ~ "Diptera"))

get_wiki(Pollinator_Data_RAW$Species)
#order level

sort(unique(Pollinator_Data_RAW$Species))

#calculate richness----
Bird_Data_filtered[, Richness := length(unique(Species)), by = list(Site, Date)]


#pivot tables to calculate diversity indices
Bird_Data_wide <- Bird_Data_filtered  %>% 
  pivot_wider(names_from = "Species", values_from = "SpCount", values_fn = sum, values_fill = 0)
