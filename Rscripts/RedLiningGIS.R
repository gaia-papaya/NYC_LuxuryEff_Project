#required packages----
require(tidyverse)
require(tidycensus)
require(sf)

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
