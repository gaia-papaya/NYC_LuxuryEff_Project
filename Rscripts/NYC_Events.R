library(tidyverse)
library(readxl)
library(data.table)
library(RColorBrewer)


setwd("~/Documents/Projects/LuxuryNYC/Analysis/Events_files")

#Get the shapefile names from the shapefile folder
events <- read_csv("Parks_Special_Events_20231208.csv")

park_events <- subset(events, LocationType %in% "Park")

my_park_events <- subset(events, Location %in% c("Pelham Bay Park", 
                                            "Van Cortlandt Park", 
                                            "Crotona Park", 
                                            "Highbridge Park", 
                                            "Inwood Hill Park", 
                                            "Soundview Park", 
                                            "Morningside Park", 
                                            "Central Park"))

my_park_events <- my_park_events %>% 
  rename("Park" = "Location")


#extract park sizes from crime dataset
crime <- read_csv("~/Documents/Projects/LuxuryNYC/Analysis/Crime_files/crimeNYC_21_23.csv")
colnames(crime)

crime2 <- crime %>% 
  rename("acres" = colnames(crime)[3])


my_park_crime <- subset(crime2, PARK %in% c("PELHAM BAY PARK", 
                                             "VAN CORTLANDT PARK" ,
                                             "CROTONA PARK" ,
                                             "HIGHBRIDGE PARK BRONX SIDE" ,
                                             "HIGHBRIDGE PARK MANHATTAN SIDE" ,
                                             "INWOOD HILL PARK" ,
                                             "SOUNDVIEW PARK" ,
                                             "MORNINGSIDE PARK" ,
                                             "CENTRAL PARK"))


#focus on size of parks
my_park_crime <- my_park_crime %>% 
  select(PARK, acres) %>% 
  distinct() #removes duplicates

#change case of park names
my_park_crime$PARK <- str_to_title(my_park_crime$PARK)
colnames(my_park_crime)<-c("Park", "acres")

#remove redundant highbridge park
my_park_crime <- my_park_crime[-which(my_park_crime$Park == "Highbridge Park Bronx Side"), ]
my_park_crime$acres[which(my_park_crime$Park== "Highbridge Park Manhattan Side")] <- 131
my_park_crime$Park[which(my_park_crime$Park== "Highbridge Park Manhattan Side")] <- "Highbridge Park"


# Join size to event dataset
my_park_events <- left_join(my_park_events, my_park_crime, by="Park")
my_park_events$acres[which(my_park_events$Park=="Central Park")] <-842.6 

#rename Event Type
my_park_events <- my_park_events %>% 
  rename("EventType" = "Event Type")

# Add column of attendees per acre
my_park_events$Attendance_acre <- my_park_events$Attendance/my_park_events$acres 

unique(my_park_events$`Event Type`)
colors<-c("#f0f9e8","#bae4bc", "#7bccc4", "#2b8cbe")

#event by type
ggplot(data=subset(my_park_events, Park!="Central Park"), aes(x=reorder(Park, Attendance, FUN=sum, na), y=Attendance, fill=EventType)) +
  geom_bar(stat='identity', color="black") +
  scale_fill_manual(values=colors) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Events 2018-2023") +
  guides(fill = guide_legend(title = "Event Type"))+
  xlab("")+
  theme_classic()

#event by type - attendance corrected by size of park 
ggplot(data=my_park_events, aes(x=reorder(Park, Attendance_acre, FUN=sum), y=Attendance_acre, fill=EventType)) +
  geom_bar(stat='identity', color="black") +
  scale_fill_manual(values=colors) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Events per acre 2018-2023") +
  guides(fill = guide_legend(title = "Event Type"))+
  xlab("")+
  theme_classic()

# number of events by size of park
my_park_events_summary <- my_park_events %>% 
  group_by(Park, acres) %>% 
  summarise(n=n())
my_park_events_summary$n_corrected<-my_park_events_summary$n/my_park_events_summary$acres


#upload cencus dataframe svi_dat<-cencus_data
#order by SVI
my_park_events_summary$order<-c(8,3,4,2,1,7,6,5)

ggplot(data=my_park_events_summary, aes(x=reorder(Park, order), y=n_corrected)) +
  geom_bar(stat="identity", aes(x=reorder(Park, order), y=n_corrected),color="black") +
#  scale_fill_manual(values=colors) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Events 2018-2023") +
#  guides(fill = guide_legend(title = "Event Type"))+
  xlab("")+
  theme_classic()

ggsave(file="~/Documents/Projects/LuxuryNYC/Events.jpg", width=3, height=4, units="in")



######################.
##### Bird Data #####
######################.


birds<-read_excel("~/Documents/Projects/LuxuryNYC/SurveyData/2023_12_08_BirdSurveys.xlsx")
birds$Site[which(birds$Site=="InnwoodHillPark")] <- "InwoodHillPark"

surveys <- birds %>%
  group_by(Site, Date) %>%
  summarise(species = length(unique(Species)))

birds$Date <- as.Date(birds$Date)

  
#Surveys done
ggplot(data=surveys, aes(x=Site)) + 
  geom_bar()+
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Surveys done") +
  xlab("")+
  theme_classic()


  
birds_summ <- birds %>% summary


