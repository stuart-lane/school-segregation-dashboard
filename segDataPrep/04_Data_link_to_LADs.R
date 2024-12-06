# Description: This script is used to link the FSM data to a dataset
# which contains the GPS coordinates of every school based on URN.
# the LAD boundaries

library(sf)
library(data.table)
library(dplyr)
library(purrr)
library(readr)
library(labelled)
library(forcats)
library(tidyverse)
library(tidyr)
library(stringr)
library(haven)

final_dir <- "1_posted/"


linkage_data <- read.csv("./school_coordinates/edubasealldata20241022.csv")
lad_boundaries <- sf::read_sf("Local_Authority_Districts_May_2024_Boundaries_UK_BUC_-7430853278415417895/")
lad_boundaries <- st_transform(lad_boundaries, crs = 31370)
# plot(lad_boundaries)

bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 
+ellps=airy +datum=OSGB36 +units=m +no_defs'

linkage_data <- linkage_data[!is.na(linkage_data$Easting) & !is.na(linkage_data$Northing),]

linkage_data <- st_as_sf(linkage_data, coords = c("Easting", "Northing"), crs = bng)
linkage_data <- st_transform(linkage_data, 31370)

combined_data <- sf::st_join(linkage_data, lad_boundaries, left = TRUE)


file_paths <- c(
  "1_posted/fsm_primary_long.dta",
  "1_posted/fsm_secondary_long.dta",
  "1_posted/race_primary_long.dta",
  "1_posted/race_secondary_long.dta"
)




for (file_path_index in 1:length(file_paths)){
  
  data <- read_dta(file_paths[file_path_index])
  
  table(data$School_Name %in% combined_data$EstablishmentName)
  table(data$urn %in% combined_data$URN==F & data$School_Name %in% combined_data$EstablishmentName==F)
  table(data$urn %in% combined_data$URN==T & data$School_Name %in% combined_data$EstablishmentName==F)
  
  linkage_indices <- c()
  reason_for_no_match <- c()
  
  # Go through each row in the dataset,
  # try and match it to a local authority
  for (row_index in 1:nrow(data)){
    # If can match by URN then do that
    if (data$urn[row_index] %in% combined_data$URN){
      new_index <- which(data$urn[row_index] %in% combined_data$URN)
    } else if (data$School_Name[row_index] %in% combined_data$EstablishmentName){
      # Otherwise use schoolname
      new_index <- which(combined_data$EstablishmentName == data$School_Name[row_index])
      
      # If there are multiple schools to link which have the same name.
      if (length(new_index) > 1){
        new_LADs <- unique(combined_data$LAD24NM[new_index])
        # If there are multiple schools to link which have the same name,
        # but are associated with different local authorities, 
        # then we have to set these to null
        if (length(new_LADs) > 1){
          new_index <- NA
          reason_for_no_match <- c(reason_for_no_match, "School Name Link")
        }
        # If there are multiple schools to link which have the same name,
        # but are associated with same local authorities, 
        # then we have to set these to zero.
        if (length(new_LADs) == 1){
          new_index <- min(new_index)
        }
      }
    }else if (data$LA_name[row_index] %in% combined_data$LA..name.){
      # If the schools are in the same local authority then check
      new_index <-  which(combined_data$LA..name. == data$LA_name[row_index])
      
      if (length(new_index) > 1){
        new_LADs <- unique(combined_data$LAD24NM[new_index])
        if (length(new_LADs) > 1){
          new_index <- NA
          reason_for_no_match <- c(reason_for_no_match, "LA Name Link")
        }
        if (length(new_LADs) == 1){
          new_index <- min(new_index)
        }
      }
    }else{
      new_index <- NA
      reason_for_no_match <- c(reason_for_no_match, "No_match")
    }
    
    if (length(new_index) > 1){
      stop("Multiple matches", data$School_Name[row_index])
    }
    
    if (!is.na(new_index)){
      reason_for_no_match <- c(reason_for_no_match, "Match")
    }
    
    
    linkage_indices <- c(linkage_indices, new_index)
    
  }
  
  data$LAD24CD <- combined_data$LAD24CD[linkage_indices]
  data$LAD24NM <- combined_data$LAD24NM[linkage_indices]
  
  write_dta(data, file_paths[file_path_index])

}


# file_paths <- c(
#   "1_posted/fsm_primary_long.dta",
#   "1_posted/fsm_secondary_long.dta",
#   "1_posted/race_primary_long.dta",
#   "1_posted/race_secondary_long.dta"
# )

# for (file_path_index in length(file_paths)){
#   data <- read_dta(file_paths[file_path_index])
#   
#   data <- merge(data, 
#                 combined_data[c("")], 
#                 by.x = "LAD24CD", 
#                 by.y = "LAD24CD", 
#                 all.x = TRUE, 
#                 all.y=F)
#   
#   
#   colnames(data) <- gsub("DistrictAdministrative..code.", "DistrictAdministrativecode24", colnames(data))
#   colnames(data) <- gsub("DistrictAdministrative..name.", "DistrictAdministrativename24", colnames(data))
#   
#   write_dta(data, file_paths[file_path_index])
#   
#   
# }




# Create a SpatialPointsDataFrame

# 
# table(data$urn[!is.na(data$urn)] %in% linkage_data$URN)
# colSums(is.na(linkage_data))
# 
# table(is.na(data$urn))
# table(is.na(data$`LAD name`))
# table(is.na(data$District))
# temp_result <- data[is.na(data$District),]
# 
# table(is.na(data$urn) & is.na(data$`LAD name`))
# table(is.na(data$`School Postcode`))
# colSums(is.na(data))
# round(100*colSums(is.na(data))/nrow(data),2)
# table(rowSums(is.na(data)))# == ncol(data) )