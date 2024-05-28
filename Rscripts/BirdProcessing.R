#dependencies----
require(tidyverse)
require(taxize)

#set up API key for NCBI
ENTREZ_KEY='a7c29a42a162d4ee6e888d77a73897e12f08'

#read in bird survey datasheet
DirtyBirdData <- read.csv("Rdata/Biodiversity/BirdSurveys_2024-05-27.csv") %>% 
mutate(UID = row_number(),#add row number as uid
      Species = str_replace_all(Species, "([:lower:])([:upper:])", "\\1 \\2"), #add space between capital letters
      #standardize dog on and off leash name
      Species = case_when(str_detect(Species, "Dog") == TRUE & str_detect(Species, "On") ~ "Dog On",
                    str_detect(Species, "Dog") == TRUE & str_detect(Species, "Off") ~ "Dog Off", 
                         TRUE ~ Species), 
    Species = str_squish(str_to_lower(Species)), #set to lower case, squish whitespace out
      #hard code values for Distance, Merlin, and Flyover columns
      Merlin = case_when(grepl("heard", .$Distance, ignore.case = T) == TRUE ~ TRUE,
                         grepl("heard", .$Notes, ignore.case = T) == TRUE ~ TRUE,
                         Merlin == "N" ~ FALSE, Merlin == "Y" ~ TRUE), 
      Flyover = case_when(Merlin == TRUE ~ NA, 
                          Flyover == "N" | Merlin == FALSE ~ FALSE,
                          Flyover == "Y"  ~ TRUE),
      Distance = case_when(grepl("merlin",.$Distance, ignore.case = T) == TRUE ~ NA, 
                           grepl("heard", .$Distance, ignore.case = T) == TRUE ~ NA, #if distance has 'merlin' or 'heard' set to NA
                           TRUE ~ Distance
      )) 

#extract unclear species names for revisitation
DirtyBirds <- DirtyBirdData %>% 
filter(Species == "sparrow"| Species =="*sparrow*"| Species == "unknown"| Species == "thrush"| Species == "vireo"| Species == "waterthrush"|
           Species == "parulidae"| Species == "laridae"| Species == "nuthatch"| Species == "gull"| Species == "finch"|
           Species == "duck" | Species == "hawk*" | Species == "grackle") %>% 
  select(UID, Site:Researcher4, TimeObs:Notes)

#filter out unclear values (those found in DirtyBirds
CleanBirdData <- DirtyBirdData %>% 
  filter(!(UID %in% DirtyBirds$UID)) 
  

#how many unique vals?
sort(unique(DirtyBirdData$Species))
sort(unique(CleanBirdData$Species))

#find scientific names for given birds
BirdSciNames <- taxize::comm2sci(unique(CleanBirdData$Species))
#for those not found on ncbi, check itis
BirdSciNamesFail <- names(Filter(is_empty,BirdSciNames))
BirdSciNamesTry2 <- taxize::comm2sci(BirdSciNamesFail, db = "itis")



comm2sci("pied billed grebe", db = "itis")
#taxize fail list:
# Pied-billed Grebe -- Podilymbus podiceps