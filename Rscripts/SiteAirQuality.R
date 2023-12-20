#This is the R script to read and process air quality files, collected from the temptop AQ monitor
#Author: Gaia
#dependencies
require(data.table)

#read filenames from AQ data folder
AQ_files <- list.files("Rdata/AQ data")

#iterate over vector to read files, send output to list
AQ_data_list <- list()
for(f in AQ_files){
  #extract site name from AQ_files
  site_name <- str_remove(stringr::str_extract(f, "(?<=[:digit:]{4}-[:digit:]{2}-[:digit:]{2}[:space:]).*(?=.csv)"), " ")
  #read csv of given file name (f)
  AQ_data_list[[f]] <- as.data.table(read.csv(paste("Rdata/AQ data/",f, sep = ""), header = T))
  #add site name to given table
  AQ_data_list[[f]][, SITE := site_name]
}
#PROGRAMMER'S NOTE: if the columns of the dataframes get messed up, try deleting the first empty column in the csv file and re run (idk why this works lol)

#merge list of dataframes into singular dataframe, reformat datetime to POSIXct format
AQ_FullData <- dplyr::bind_rows(AQ_data_list)
AQ_FullData[,DATE := as.POSIXct(DATE)]

#remove intermediate objects
rm(AQ_data_list, site_name, f, AQ_files)
