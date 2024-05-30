#dependencies----
require(tidyverse)
require(taxize)

#set up API key for NCBI using usethis::edit_r_environ()
#ENTREZ_KEY='a7c29a42a162d4ee6e888d77a73897e12f08'

#read in bird survey datasheet
DirtyBirdData <- read.csv("Rdata/Biodiversity/BirdSurveys_2024-05-29.csv") %>% 
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
  filter(!(UID %in% DirtyBirds$UID))  %>% 
  rename(commonNames = Species)
  

#how many unique vals?
sort(unique(DirtyBirdData$Species))
sort(unique(CleanBirdData$Species))

#find scientific names for given birds
BirdSciNames <- taxize::comm2sci(unique(CleanBirdData$commonNames[CleanBirdData$commonNames != "dog on" & CleanBirdData$commonNames != "dog off"]))
BirdSciNamesFail <- names(Filter(is_empty,BirdSciNames))
BirdSciNames <- compact(BirdSciNames)

#for those not found on ncbi, check eol
BirdSciNamesTry2 <- taxize::comm2sci(BirdSciNamesFail, db = "eol")
BirdSciNamesFail2 <- names(Filter(is_empty,BirdSciNamesTry2))
BirdSciNamesTry2 <- compact(BirdSciNamesTry2)
#append onto one list
BirdSciNames <- c(BirdSciNames, BirdSciNamesTry2)

#assign correct names into data frame
inconclusiveNames <- list()
for(n in 1:length(BirdSciNames)){
  if(length(BirdSciNames[[n]]) > 1){
  inconclusiveNames[[n]] <- BirdSciNames[[n]]
  names(inconclusiveNames)[[n]] <- names(BirdSciNames)[[n]]
 
}
}
#names with multiple results, manual fix
inconclusiveNames<- compact(inconclusiveNames)
RepairedNames <- list("brown creeper" = inconclusiveNames$`brown creeper`[2],
                      "ruby crowned kinglet" = inconclusiveNames$`ruby crowned kinglet`[1],
                      "winter wren" = inconclusiveNames$`winter wren`[1],
                      "pied billed grebe" = inconclusiveNames$`pied billed grebe`[1],
                      "brant" = inconclusiveNames$brant[5],
                      "eastern towhee" = inconclusiveNames$`eastern towhee`[1],
                      "blue grosbeak" = inconclusiveNames$`blue grosbeak`[1],
                      "brownheaded cowbird" = 'Molothrus ater') 
#remove inconclusive names from list
BirdSciNames <- within(BirdSciNames, rm(list =names(inconclusiveNames)))
#create tibble with both common names and scientific names
BirdSciNamesTbl <- tibble(commonNames = names(BirdSciNames), species = unlist(BirdSciNames))

#create tibble with both common names and scientific names
RepairedNamesTbl <- tibble(commonNames = names(RepairedNames), species = unlist(RepairedNames))

#merge two tibbles, remove intermediaries
CommSciBirdTbl <- bind_rows(BirdSciNamesTbl, RepairedNamesTbl)
rm(BirdSciNames, BirdSciNamesTbl, BirdSciNamesFail, BirdSciNamesFail2, BirdSciNamesFail3,
   BirdSciNamesTry2, BirdSciNamesTry3, RepairedNames, BirdSciNamesTbl)
#query full taxonomic classif
TaxonClass <- taxize::classification(CommSciBirdTbl$species, db = "ncbi")

#select only the ranks of order, family, and genus
for(t in 1:length(TaxonClass)){
  TaxonClass[[t]] <- TaxonClass[[t]] %>% 
    select(!id) %>% 
    filter(rank =="order" | rank == "family" | rank == "genus" | rank == "species") %>% 
    pivot_wider(names_from = "rank", values_from = "name")
  
}
#bind into a single data frame
TaxonClassTbl <- do.call(rbind, TaxonClass)

#join table with common names to full classif table
TaxonClassTbl <- full_join(TaxonClassTbl, CommSciBirdTbl, by = "species")

#join classif table to full data sheet
CleanBirdData <- full_join(CleanBirdData, TaxonClassTbl, by = "commonNames", relationship = "many-to-one")
