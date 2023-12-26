library(data.table)
library(raster)
library(rgdal)
library(sf)
library(tidyverse)
library(readxl)


######################################.
#### Proccess & merge raw files   ####
######################################.


setwd("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/Crime_files")

#Get the shapefile names from the shapefile folder
crime_list <- list.files(pattern="\\.xlsx")  #folder with all files is named "Files"

outlist <- list()
for (i in 1:length(crime_list)){
  
  #get date from filenames
  file_names <- data.frame(file = crime_list[i])
  file_names <- file_names %>% 
    mutate(file=file) %>% 
    separate(file, into = c("drop1","drop2", "drop3", "drop4", "quarter", "year"), sep = "-") %>% 
    separate(year, into = c("year","drop5"), sep = ".xlsx")
  file_info<-file_names %>% select(quarter, year)         
  
  #read in df
  dat<-read_excel(crime_list[i], skip = 3, col_types = c("text", "text", "numeric", "text", 
                                                         "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric", "numeric", 
                                                        "numeric", "numeric"))
  
  #add file info to df
  dat$YEAR <- file_info$year
  dat$QUARTER <- file_info$quarter
  
  outlist[[i]] <- dat 
  
}

crime_data <- data.table::rbindlist(outlist)

write_csv(crime_data, "crimeNYC_21_23.csv")

##########################################.
#### Read in data - Select for parks  ####
##########################################.

#read in data
crime <- read_csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/Crime_files/crimeNYC_21_23.csv")


park_crime <- crime_data[which(PARK=="PELHAM BAY PARK" | 
                           PARK=="VAN CORTLANDT PARK" |
                           PARK=="CROTONA PARK" |
                           PARK=="HIGHBRIDGE PARK BRONX SIDE" |
                           PARK=="HIGHBRIDGE PARK MANHATTAN SIDE" |
                           PARK=="INWOOD HILL PARK" |
                           PARK=="SOUNDVIEW PARK" |
                           PARK=="MORNINGSIDE PARK" |
                           PARK=="VAN CORTLANDT PARK" |
                           PARK=="CENTRAL PARK"),]

#Cleanup
park_crime$`SIZE (ACRES)` <- signif(park_crime$`SIZE (ACRES)`, digits = 3) #standardize acre numbers
park_crime$PARK <- str_to_title(park_crime$PARK) #change case of park names
colnames(park_crime) <- str_to_title(colnames(park_crime)) #change case of column names
colnames(park_crime)[3] <- "Acres"

park_crime$Park[which(park_crime$Park=="Highbridge Park Manhattan Side")] <- "Highbridge"
park_crime$Park[which(park_crime$Park=="Crotona Park")]<-"Crotona"
park_crime$Park[which(park_crime$Park=="Inwood Hill Park")]<-"Inwood"
park_crime$Park[which(park_crime$Park=="Soundview Park")]<-"Soundview"
park_crime$Park[which(park_crime$Park=="Van Cortlandt Park")]<-"VanCortlandt"
park_crime$Park[which(park_crime$Park=="Morningside Park")]<-"Morningside"
park_crime$Park[which(park_crime$Park=="Pelham Bay Park")]<-"PelhamBaySouth"


park_crime_21_23 <- park_crime %>% group_by(Park, Borough, Acres) %>%
  summarise(murder = sum(Murder), robbery=sum(Robbery), assault=sum(`Felony Assault`), 
            burglary=sum(Burglary), larcen=sum(`Grand Larceny`), larcen_motor=sum(`Grand Larceny Of Motor Vehicle`),
            crime_total=sum(Total))


#write_csv(park_crime_21_23, "~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/park_crime_21_23.csv")
#read in park specific data
#park_crime_21_23 <- readcsv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/output/park_crime_21_23.csv")


#make long
park_crime_21_23_long <- park_crime_21_23  %>%
  gather(key = "n", value = "Value")

park_crime_21_23_long <- park_crime_21_23 %>%
  pivot_longer(
    cols = c("murder","robbery","assault","burglary","larcen","larcen_motor"), 
    names_to = c("Crime"),  # Create two new columns for Variable and Year
    values_to = ("N_crime")  # Name for the new column with values
  )

#Add crime per acre
park_crime_21_23_long$crime_per_acre <- park_crime_21_23_long$crime_total/park_crime_21_23_long$Acres

#Add SVI
cencus_data <- read.csv("~/Documents/Projects/LuxuryNYC/NYC_LuxuryEff_Project/Rdata/SVI_df.csv")
    #merge park names
    unique(park_crime_21_23_long$Park)
    unique(cencus_data$park) 
    colnames(cencus_data)[4] <- "Park"
    cencus_data$Park[which(cencus_data$Park=="HighbridgeMAN")]<-"Highbridge"
    cencus_data$Park[which(cencus_data$Park=="Pelham")]<-"PelhamBay"
    cencus_data$Park[which(cencus_data$Park=="Vancortlandt")]<-"VanCortlandt"
cencus_data_summary<-cencus_data %>% group_by(Park) %>%
  summarise(SVI=mean(SVI), SocioEco=mean(SocioEco), Minority=mean(Minority))
    

park_crime_21_23_long <- left_join(park_crime_21_23_long, cencus_data_summary,  by = c("Park"))
#remove highbridge bronx
park_crime_21_23_long2<-subset(park_crime_21_23_long, Park!="Highbridge Park Bronx Side")

###################.
####   PLOTS   ####
###################.

park_crime_21_23_long2<-subset(park_crime_21_23_long, Park!="Highbridge Park Bronx Side") #removing highbridge bronx)

#total crime
ggplot(data=park_crime_21_23_long2, aes(x=reorder(Park, SVI), y=crime_total)) + 
  geom_bar(stat='identity')+
 # stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
#               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Total Crime 2021-2023") +
  xlab("")+
  theme_classic()

#crime adjusted for size 
ggplot(data=park_crime_21_23_long2, aes(x=reorder(Park, SVI), y=crime_per_acre)) + 
  geom_bar(stat='identity')+
  # stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
  #               geom = "pointrange", color = "black") +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Crime Per Acre 2021-2023") +
  xlab("")+
  theme_classic()


colors<-c("#fef0d9", "#fdd49e",  "#fdbb84", "#fc8d59", "#e34a33", "#b30000")
  
#crime by type
ggplot(data=park_crime_21_23_long2, aes(x=reorder(Park, SVI), y=N_crime, fill=Crime)) +
  geom_bar(stat='identity', color="black") +
  scale_fill_manual(values=colors) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Crime 2021-2023") +
  guides(fill = guide_legend(title = "Crime Type"))+
  xlab("")+
  theme_classic()


# CRIME X SVI

park_crime_21_23_long2 %>% group_by(Park, SVI, SocioEco, Minority, crime_per_acre, N_crime, crime_total, Acres) %>%

ggplot(aes(x=SVI, y=crime_per_acre)) + 
  geom_point(aes(x=SVI, y=crime_total))+
  # stat_summary(fun.data = "mean_sdl",  fun.args = list(mult = 1), 
  #               geom = "pointrange", color = "black") +
#  scale_x_discrete(guide = guide_axis(angle = 45))+
  ylab("Total Crime 2021-2023") +
 # xlab("")+
  theme_classic()

