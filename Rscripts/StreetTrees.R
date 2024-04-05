require(tidyverse)
streetTrees <- read_csv("Rdata/GIS/trees.csv") %>%
  group_by(GenusSpecies) %>% 
  summarise(count = n())
