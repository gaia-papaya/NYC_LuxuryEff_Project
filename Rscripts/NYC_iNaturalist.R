
library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(dplyr)
library(stringdist)

#reads in dataframes extracted from inaturalist from parks (2019-01-01 to 2023-12-20)
#combines them into a single dataframe 

setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/iNaturalist")



#####################################.
#### Proccess raw iNAT downloads ####
#####################################.

# List all iNat files in the directory
inat_list <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/iNaturalist", 
                             pattern = "\\.csv$", full.names = TRUE)

#get date from filenames
parklist <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/iNaturalist", 
                   pattern = "\\.csv$")


#reduce and combine datasets
outlist <- list()
for (i in 1:length(inat_list)){
  
  #read in df
  dat<-read_csv(inat_list[i])
  dat <- dat %>% select(observed_on, user_id, user_login, place_guess,
                        latitude, longitude, species_guess, scientific_name, 
                        common_name, iconic_taxon_name, num_identification_disagreements, 
                        num_identification_agreements) %>%
    mutate(locality = place_guess, eventDate=as.character(observed_on))
    dat$observed_on<-as.character(dat$observed_on)
  
  #add park name to df
  file_name <- data.frame(file = parklist[i])
  file_name <- file_name %>% 
  mutate(file=file) %>% 
  separate(file, into = c("park","source"), sep = "_") %>%
    separate(source, into = c("source","drop")) 

  dat$park <- file_name$park
  dat$source <- file_name$source
        
  outlist[[i]] <- dat 
  
}

inat_df <- bind_rows(outlist)

#remove ourselves from observations
inat_df<-inat_df[which(inat_df$user_login!="gaia_papaya"),]
inat_df<-inat_df[which(inat_df$user_login!="kmwinchell"),]
inat_df<-inat_df[which(inat_df$user_login!="anubesh"),]
inat_df<-inat_df[which(inat_df$user_login!="calcariidaeeee"),]


#remove mammals (error in downloading data)
inat_df<-inat_df[which(inat_df$iconic_taxon_name!="Mammalia"),]

#remove observations where disagreements > 0.5* agreements 
#which(inat_df$num_identification_agreements>1 & inat_df$num_identification_disagreements> (0.5*inat_df$num_identification_agreements))

#modify park name
inat_df$park[which(inat_df$park=="PelhamBay")]<-"PelhamBaySouth"


#save combined df
write_csv(inat_df, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/inat_2019-23.csv")


#####################################.
#### Proccess raw ebird downloads ####
#####################################.

# List all iNat files in the directory
ebird_list <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/eBird", 
                        pattern = "\\.csv$", full.names = TRUE)

#get date from filenames
parklist <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/eBird", 
                       pattern = "\\.csv$")


#reduce and combine datasets
outlist <- list()
for (i in 1:length(ebird_list)){
  
  #read in df
  dat<-read_delim(ebird_list[i])
  dat <- dat %>% select(kingdom, phylum, class, order, family, genus, species, 
                        verbatimScientificName, locality, decimalLatitude,
                        decimalLongitude, eventDate, year, recordedBy) %>%
    mutate(latitude = decimalLatitude, longitude = decimalLongitude, 
           user_login = recordedBy,iconic_taxon_name = class, 
           scientific_name = verbatimScientificName,
           common_name = species
           )
  
  #add park name to df
  file_name <- data.frame(file = parklist[i])
  file_name <- file_name %>% 
    mutate(file=file) %>% 
    separate(file, into = c("park","source"), sep = "_") %>%
    separate(source, into = c("source","drop")) 
  
  dat$park <- file_name$park
  dat$source <- file_name$source
  
  outlist[[i]] <- dat 
  
}

ebird_df <- bind_rows(outlist)


#remove ourselves from observations
#ebird_df<-ebird_df[which(ebird_df$user_login!="gaia_papaya"),]
#ebird_df<-ebird_df[which(ebird_df$user_login!="kmwinchell"),]
#ebird_df<-ebird_df[which(ebird_df$user_login!="anubesh"),]

#save combined df
write_csv(ebird_df, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ebird_2019-23.csv")



#################################.
#### Read & Combine datasets ####
#################################.


#read in inat and ebird and tree diversity dfs
inat_df <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/inat_2019-23.csv")
ebird_df <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/ebird_2019-23.csv")
parktrees <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/parktrees_2016-2023.csv")
    parktrees$park[which(parktrees$park=="Inwood")]<-"InwoodHill"
    parktrees <- parktrees %>%
      mutate(common_name=common, scientific_name = scientific, user_login="NA", 
             source="tree_points_nyc", iconic_taxon_name = "Plantae")

    
#determine effort - eventDate per userID per park
inat_num_events <- inat_df %>%
  group_by(park) %>%
  summarize(num_events = n_distinct(paste(user_login, eventDate, sep = "_"))) %>%
  mutate(source="inat")
ebird_num_events <- ebird_df %>%
  group_by(park) %>%
  summarize(num_events = n_distinct(paste(user_login, eventDate, sep = "_"))) %>%
  mutate(source="ebird")

test<-inat_df
test$num_events<-NA
for (j in nrow(inat_num_events)){
  for (i in nrow(test)){
    if (test$park[i]==inat_num_events$park[j]){
      test$num_events[i] <-inat_num_events$num_events[j]
    }
  }
}

##### Clip to parks (for some where we only sample a portion) #####

# List all shapefiles in the directory
#shapefile_list <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2", 
                             pattern = "\\.shp$", full.names = TRUE)

#get name from filenames (originals)
#shapefile_names <- list.files(path = "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/GIS/ParkShapeFiles_Raf2", 
#                              pattern = "\\.shp$")
#shape_file_names <- data.frame(file = shapefile_names)
#shape_file_names <- shape_file_names %>% 
#  mutate(file=file) %>% 
#  separate(file, into = c("park","drop1"), sep = ".shp")

#get crs from park shape files
#park_crs <- crs(st_read(shapefile_list[1]))


#df_sf <- st_as_sf(inat_df, coords = c("longitude", "latitude"), crs=4326)
#df_sf <- st_transform(df_sf, crs = park_crs)



#result_list <- list()
#for (i in 1:length(shapefile_list)) {
#  
##  # Read the shapefile
#  shp <- st_read(shapefile_list[i])
  
  # Use st_join to keep only the points within the polygons
#  clipped <- st_intersection(df_sf, shp)
  
#  #add park name to df
#  clipped$park <- shape_file_names$park[i]
#  
#  # Append the result to the list
#  result_list[[i]] <- clipped
#  print(i)
#}

# Combine the results into a single sf object
#inat_parks_df <- data.table::rbindlist(result_list)




##### combine ebird, inat, and tree cencus

common_cols<-intersect(colnames(inat_df), colnames(parktrees))
inat_ebird_df <- rbind(
  subset(inat_df, select = common_cols), 
  subset(ebird_df, select = common_cols),
  subset(parktrees, select = common_cols)
)


##### search for typos ####

species <- as.data.frame(unique(inat_ebird_df$scientific_name))
colnames(species)<-"scientific_name"

    
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

inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Desmodium canescens")] <-  "Desmodium canescense"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Passerella iliaca iliaca")] <-  "Passerella iliaca"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Equisetum hyemale affine")] <-  "Equisetum hyemale"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Toxicodendron radicans radican")] <-  "Toxicodendron radicans"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Gleditsia triacanthos var. inermis 'Skyline'")] <-  "Gleditsia triacanthos var. inermis"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Gleditsia triacanthos var. inermis 'Halka'")] <-  "Gleditsia triacanthos var. inermis"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Nycticorax nycticorax hoactli'")] <-  "Nycticorax nycticorax"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Leiothlypis celata celata'")] <-  "Leiothlypis celata"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Liriodendron tulipifera 'Arnold'")] <-  "Liriodendron tulipifera"
inat_ebird_df$scientific_name[which(inat_ebird_df$scientific_name=="Chrysemys picta picta")] <-  "Chrysemys picta"


#### calculate richness  ####
### also accounting for total observations ###

#by group
all_richness <- inat_ebird_df %>%
  group_by(park, iconic_taxon_name) %>%
  summarize(unique_sp = length(unique(scientific_name)), n_observations=)

#Including data source grouping  (ebird, inat, nyc trees)
all_richness2 <- inat_ebird_df %>%
  group_by(park, iconic_taxon_name, source) %>%
  summarize(unique_sp = length(unique(scientific_name)))

#add groups for herps, plants, inverts
all_richness$taxon<-NA
all_richness$taxon[which(all_richness$iconic_taxon_name=="Reptilia" | all_richness$iconic_taxon_name=="Amphibia")]<-"Herpetofauna"
all_richness$taxon[which(all_richness$iconic_taxon_name=="Insecta" | all_richness$iconic_taxon_name=="Arachnida")]<-"Ter. Inverts"
all_richness$taxon[which(all_richness$iconic_taxon_name=="Plantae" | all_richness$iconic_taxon_name=="trees")]<-"Plantae"
all_richness$taxon[which(all_richness$iconic_taxon_name=="Aves")] <- "Aves"


#inat_richness <- inat_df %>%
#  group_by(park, iconic_taxon_name, source) %>%
#  summarize(unique_sp = length(unique(scientific_name)))

#ebird_richness <- ebird_df %>%
#  group_by(park, iconic_taxon_name, source) %>%
#  summarize(unique_sp = length(unique(scientific_name)))

#tree_richness <- parktrees %>%
#  group_by(park) %>%
#  summarize(unique_sp = length(unique(scientific)), iconic_taxon_name="trees") %>%
#  mutate(source="tree_points_nyc")

#all_richness <-rbind(inat_richness, ebird_richness, tree_richness) 
  

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
combo<-left_join(all_richness, cencus_data_summary, by="park")
combo<-left_join(combo, crime, by="park") %>%
  mutate(sp_rich_area=(unique_sp/Acres))


                 
#correct for # of observations



#Plots
combo<-combo[-which(combo$park=="CentralPark"),]

#all taxa
ggplot(data=combo, aes(x=reorder(park, SVI), y=sp_rich_area, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Species Richness / area") +
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  xlab("")+
  ggtitle("All taxa") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))

#by taxonomic group
ggplot(data=combo, aes(x=reorder(park, SVI), y=sp_rich_area, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Species Richness / area") +
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  xlab("")+
#  ggtitle("All taxa") +
  facet_grid(iconic_taxon_name ~ ., scales = "free_y") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5))


#combine herps, plants, inverts
ggplot(data=combo, aes(x=reorder(park, SVI), y=sp_rich_area, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Species Richness / area") +
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  xlab("")+
#  ggtitle("All taxa") +
  facet_grid(taxon ~ ., scales = "free_y") +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm"))
ggsave(filename="rich_svi_bars.jpg", width = 5, height = 7, units = c("in"), dpi = 300, 
       path = "~/Documents/Projects/LuxuryNYC/Figures")



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

ggsave(filename="rich_svi_trend.jpg", width = 5, height = 4, units = c("in"), dpi = 300, 
       path = "~/Documents/Projects/LuxuryNYC/Figures")







ggplot(data=combo, aes(x=reorder(park, SVI), y=tree_richness_per_acre, fill=SVI)) +
  geom_bar(stat="identity", color="black")+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_fill_gradient(high = "#800000", low = "#EEE8AA")+
  ylab("Tree Species Richness / area") +
  xlab("") +
  theme_classic()

ggsave(filename="inat_richness.jpg", width = 5, height = 4, units = c("in"), dpi = 300, 
    path = "~/Documents/Projects/LuxuryNYC/Figures")
