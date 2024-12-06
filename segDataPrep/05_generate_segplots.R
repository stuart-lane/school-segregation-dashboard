### ============================================================================
### SETUP
### ============================================================================

library(janitor)
library(data.table)
library(haven)
library(here)
library(ggplot2)
library(patchwork)
library(readstata13)
library(devtools)
library(dplyr)
library(gridExtra)
library(extrafont)
library(ggtext)
library(showtext)
library(tidyverse)
library(Rcpp)
library(checkmate)
library(segregation)
library(jcolors)

# Install and load GitHub packages

# Register fonts for Windows bitmap output and set default font
if (.Platform$OS.type == "windows") {
  extrafont::loadfonts(device = "win")
  windowsFonts(Times = windowsFont("Times New Roman"))
}

# Define location for temporary .csv files
tmp_dir <- "./segData/Prep/99_tmp/"

### ============================================================================
### SET COLOUR SCHEME
### ============================================================================

display_jcolors("pal3")
jcolors('default')
jcolors('pal3')
colorrace <- c("White" = "#F5E400", "Asian" = "#009ACD", "Black" = "#FF4500")
colorfsm <- c("FSM eligible" = "#E901AF", "Not FSM eligible" = "#01A8E6")

### ============================================================================
### LOOP OVER ALL SCHOOL AND SEGREGATION CATEGORIES FOR 2009/10 - 2023/24
### (going to need to revisit for earlier years as District data no available)
### ============================================================================

year_codes <-  c("2023_24", "2022_23", "2021_22", "2020_21", "2019_20",
                 "2018_19", "2017_18", "2016_17", "2015_16", "2014_15",
                 "2013_14", "2012_13", "2011_12", "2010_11", "2009_10")

# Define directory paths for figures
school_categories <- c("primary", "secondary")
seg_categories <- c("race", "fsm")

# Create an empty data frame to store error information
error_log <- data.frame(
  year_code = character(),
  district_name = character(),
  message = character(),
  stringsAsFactors = FALSE
)

for (year_code in year_codes) {
  
  year_code_title <- gsub("_", "-", year_code)
  
  for (school_category in school_categories) {
    
    for (seg_category in seg_categories) {
      
      if (seg_category == "fsm") {
        seg_category_title <- toupper(seg_category)
      } else {
        seg_category_title <- seg_category
      }
      
      ### PRELIMINARIES --------------------------------------------------------
      
      if (school_category == "primary") {
        color_scheme <- colorrace
      } else {
        color_scheme <- colorfsm
      }
      
      if (seg_category == "race") {
        color_scheme <- colorrace
      } else {
        color_scheme <- colorfsm
      }
      
      ### SET UP FIGURE DIRECTORIES --------------------------------------------
      
      fig_folder <- paste0(school_category, "_", seg_category)
      fig_path <- paste0("2_figures_new/", year_code, "/", fig_folder)
      
      if (!file.exists(fig_path)) {
        dir.create(fig_path, recursive = TRUE)
        cat("Directory created:", fig_path, "\n")
      } else {
        cat("Directory already exists:", fig_path, "\n")
      }
      
      # Create figure names
      # file_name <- paste(seg_category, school_category, "long_matched.dta", sep = "_")
      file_name <- paste(seg_category, school_category, "long.dta", sep = "_")
      
      file_path <- paste("1_posted", file_name, sep = "/")
      fig_label <- paste0(school_category, "_", seg_category, "_", year_code)
      
      ### LOAD DATA ------------------------------------------------------------
      
      # Import .dta file
      data <- read_dta(file_path)
      data_table <- as.data.table(data)
      
      data_table <- data_table %>%
        mutate(across(where(is.labelled), as_factor))
      
      # Filter data for current year
      data_table_district <- data_table[year == year_code]
      
      ### LOOP OVER DISTRICTS --------------------------------------------------
      
      district_names <- sort(unique(data_table[["LAD24NM"]]))
      district_names <- district_names[district_names != ""]
      
      # Loop through each district
      for (district_name in district_names) {
        
        # Filter data for current district
        data_table_district <- data_table[District == district_name]
        
        # Filter data for current year
        # data_table_district <- data_table_district[year == year_code]
        
        # Attempt to generate plot and handle errors
        result <- tryCatch({
          # Generate segregation plot
          fig <- segplot(data_table_district, seg_category, "School_Name",
                         weight = "n",
                         order = if(seg_category == "fsm") "majority" else "segregation") +
            theme(text = element_text(family = "Times"),
                  plot.title = element_text(hjust = 0.5)
            ) +
            scale_fill_manual(values = color_scheme) +
            labs(title = paste0(district_name,  ": State-funded ", school_category,
                                " schools ", seg_category_title, " segregation (",
                                year_code_title, " )"))
          
          # Remove spaces and convert to lower case for file naming
          fig_tag <- tolower(gsub(" ", "_", district_name))
          
          # Save figure
          ggsave(paste0(fig_path, "/", fig_tag, "_", fig_label, ".png"),
                 plot = fig, width = 9, height = 5, device = "png", dpi = 300)
          
        }, error = function(e) {
          error_log <- rbind(error_log, data.frame(
            year_code = year_code,
            district_name = district_name,
            message = e$message,
            stringsAsFactors = FALSE
          ))
          
          cat("Error for district:", district_name, "\n")
          cat("Message:", e$message, "\n")
          NULL
        })
      }
      
      # END OF LOOP ------------------------------------------------------------
      
    }
    
  }
  
}
