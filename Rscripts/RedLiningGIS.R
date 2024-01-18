#required packages----
require(tidyverse)
require(tidycensus)
require(sf)

#load redlining csv
RedLine <- read_csv("Rdata/GIS/500mIntersectRedline.csv") %>% 
  mutate(gradeScore = case_when(grade == "A" ~ 1, #assign grade scores based on grade, with poorer grades = higher score
                                grade == "B" ~ 2,
                                grade == "C" ~ 3,
                                grade == "D" ~ 4,
                                grade == "E" ~ 5,
                                grade == NA ~ NA))

#Plot EDA----

ggplot(data = RedLine, aes(x = name311, y = gradeScore)) +
  geom_violin() +
  stat_summary(geom = "point", fun.y = mean)




#tidycensus bullshit----
#load variables bc US census uses the worst system know to humanity----
vars_avail<- load_variables(year = 2021, dataset = "acs1")

#loading census track data for nyc ----
Manhattan_census <- get_acs(
geography = 'tract',
state = "NY",
county = "New York",
year = 2021,
geometry = T,
variables = "B19001_001"
)

Bronx_census <- get_acs(
  geography = 'tract',
  state = "NY",
  county = "Bronx",
  year = 2021,
  geometry = T,
  variables = "B19001_001"
)

#plot sf layer of nyc
ggplot()+
  geom_sf(data = Manhattan_census, aes(fill= estimate)) +
  geom_sf(data = Bronx_census, aes(fill = estimate))
