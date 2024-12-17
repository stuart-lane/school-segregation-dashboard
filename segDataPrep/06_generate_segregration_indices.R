### ============================================================================
### ============================================================================
### ADD DISSIMILARITY INDICES
### ============================================================================
### ============================================================================

library(labelled)
library(dplyr)
library(tidyr)
library(haven)
library(tidyverse)
library(data.table)

# setwd("C:/Users/sl14120/OneDrive - University of Bristol/Seg")

seg_folder <- "3_segregation_indices/"
single_index_folder <- paste0(seg_folder, "single_index/")
panel_folder <- paste0(seg_folder, "panel/")

folders <- c(seg_folder, single_index_folder, panel_folder)

for (folder in folders) {
  dir.create(folder, showWarnings = FALSE, recursive = TRUE) 
}

### ============================================================================
### SET-UP COMMON VARIABLES
### ============================================================================

school_types <- c("primary", "secondary")

years <- c("2008_09", "2009_10", "2010_11", "2011_12", "2012_13",
           "2013_14", "2014_15", "2015_16", "2016_17")

get_labels <- function(x) {
  if (is.factor(x)) {
    return(as.character(x))
  } else {
    return(x)
  }
}

race_groups <- list(
  c("Asian", "Black"),
  c("Asian", "White"),
  c("Black", "White")
)

tag <- "_long_matched.dta"
tag <- "_long.dta"

### ============================================================================
### FSM DISSIMILARITY INDICES
### ============================================================================

for (school_type in school_types) {
  
  dta_file <- paste0("1_posted/fsm_", school_type, tag)  
  
  data <- read_dta(dta_file)
  data$District_Name <- as.character(as_factor(data$LAD24NM))
  # data$District_Name <- ifelse(is.na(data$District_Name),
  #                              data$LA_name, data$District_Name)
  
  school_data <- data %>%
    group_by(year, District_Name, School_Name, fsm) %>%
    summarise(total_students = sum(n, na.rm = TRUE)) %>%
    ungroup()
  
  # Modify the dissimilarity function to work with grouped data
  calculate_dissimilarity_for_group <- function(data) {
    # Separate FSM and Not FSM data
    fsm_data <- data %>% filter(fsm == 1) %>% select(School_Name, total_students)
    not_fsm_data <- data %>% filter(fsm == 2) %>% select(School_Name, total_students)
    
    # Merge FSM and Not FSM data at the school level
    merged_data <- merge(fsm_data, not_fsm_data, by = "School_Name",
                         all = TRUE, suffixes = c("_FSM", "_NotFSM"))
    
    # Replace NA with 0 for schools that might not have both FSM and Not FSM students
    merged_data[is.na(merged_data)] <- 0
    
    # Total number of FSM and Not FSM students in the group
    total_FSM <- sum(merged_data$total_students_FSM, na.rm = TRUE)
    total_NotFSM <- sum(merged_data$total_students_NotFSM, na.rm = TRUE)
    
    # Calculate the dissimilarity index using the formula
    dissimilarity_index <- 0.5 * sum(abs(merged_data$total_students_FSM / total_FSM -
                                           merged_data$total_students_NotFSM / total_NotFSM),
                                     na.rm = TRUE)
    
    return(dissimilarity_index)
  }
  
  # Create the new table with dissimilarity index for each year-district combination
  dissimilarity_table <- school_data %>%
    group_by(year, District_Name) %>%
    group_modify(~ tibble(dissimilarity_index = calculate_dissimilarity_for_group(.x))) %>%
    ungroup()
  
  # View the resulting table
  print(dissimilarity_table)
  
  save_file <- paste0(single_index_folder, "dissimilarity_fsm_", school_type, ".csv")
  
  write.csv(dissimilarity_table, save_file, row.names = FALSE)
  
}

### ============================================================================
### PAIRWISE RACE DISSIMILARITY INDICES
### ============================================================================

for (school_type in school_types) {
  
  dta_file <- paste0("1_posted/race_", school_type, tag)  
  
  data <- read_dta(dta_file)
  data <- as.data.table(data)
  
  data$District_Name <- as.character(as_factor(data$LAD24NM))
  # data$District_Name <- ifelse(is.na(data$District_Name),
  #                              data$LA_name, data$District_Name)
  
  school_data <- data %>%
    group_by(year, District_Name, School_Name, race) %>%
    summarise(total_students = sum(n, na.rm = TRUE)) %>%
    ungroup()
  
  
  school_data$race_data <- ifelse(school_data$race == 1, "White",
                                  ifelse(school_data$race == 2, "Asian", "Black"))
  
  for (race_group in race_groups) {
    
    group_1 <- race_group[1]
    group_2 <- race_group[2]
    
    school_data_filtered <- school_data %>%
      filter(race_data %in% c(group_1, group_2))
    
    # Modify the dissimilarity function to work with grouped data
    calculate_dissimilarity_for_group <- function(data) {
      # Separate FSM and Not FSM data
      group_1_data <- data %>% filter(race_data == group_1) %>% select(School_Name, total_students)
      group_2_data <- data %>% filter(race_data == group_2) %>% select(School_Name, total_students)
      
      # Merge FSM and Not FSM data at the school level
      merged_data <- merge(group_1_data, group_2_data, by = "School_Name",
                           all = TRUE,
                           suffixes = c("_group1", "_group2"))
      
      # Replace NA with 0 for schools that might not have both FSM and Not FSM students
      merged_data[is.na(merged_data)] <- 0
      
      # Total number of FSM and Not FSM students in the group
      total_group1 <- sum(merged_data$total_students_group1, na.rm = TRUE)
      total_group2 <- sum(merged_data$total_students_group2, na.rm = TRUE)
      
      # Calculate the dissimilarity index using the formula
      dissimilarity_index <- 0.5 * sum(abs(merged_data$total_students_group1 / total_group1 -
                                             merged_data$total_students_group2 / total_group2),
                                       na.rm = TRUE)
      
      return(dissimilarity_index)
    }
    
    # Create the new table with dissimilarity index for each year-district combination
    dissimilarity_table <- school_data_filtered %>%
      group_by(year, District_Name) %>%
      group_modify(~ tibble(dissimilarity_index = calculate_dissimilarity_for_group(.x))) %>%
      ungroup()
    
    # View the resulting table
    print(dissimilarity_table)
    
    save_file <- paste0(single_index_folder, "dissimilarity_race_", tolower(group_1), "_",
                        tolower(group_2), "_", school_type, ".csv")
    
    write.csv(dissimilarity_table, save_file, row.names = FALSE)
    
  }
  
}

### ============================================================================
### THEIL ENTROPY FOR RACE
### ============================================================================

for (school_type in school_types) {
  
  dta_file <- paste0("1_posted/race_", school_type, tag)  
  data <- read_dta(dta_file)
  
  data$District_Name <- as.character(as_factor(data$LAD24NM))
  # data$District_Name <- ifelse(is.na(data$District_Name),
  #                              data$LA_name, data$District_Name)
  
  school_data <- data %>%
    group_by(year, District_Name, School_Name, race) %>%
    summarise(total_students = sum(n, na.rm = TRUE)) %>%
    ungroup()
  
  # Assign racial group labels to race_data
  school_data$race_data <- ifelse(school_data$race == 1, "White",
                                  ifelse(school_data$race == 2, "Asian", "Black"))
  
  # Calculate the Theil index for each district-year combination
  calculate_theil_entropy_index <- function(data) {
    # Total students at the district level
    total_students_district <- sum(data$total_students, na.rm = TRUE)
    
    # Calculate total students per school
    total_students_school <- data %>%
      group_by(School_Name) %>%
      summarise(school_total = sum(total_students, na.rm = TRUE))
    
    # Merge school totals with racial data
    data <- merge(data, total_students_school, by = "School_Name")
    
    # Calculate overall proportions for each race in the district
    race_proportions_district <- data %>%
      group_by(race_data) %>%
      summarise(race_total = sum(total_students, na.rm = TRUE)) %>%
      mutate(race_proportion = race_total / total_students_district)
    
    # Merge race proportions with main data
    data <- merge(data, race_proportions_district, by = "race_data")
    
    # Calculate Theil Index
    theil_entropy_index <- data %>%
      mutate(
        proportion_school = school_total / total_students_district,
        proportion_race_school = total_students / school_total,
        log_ratio = ifelse(proportion_race_school > 0 & race_proportion > 0,
                           log(proportion_race_school / race_proportion),
                           0),
        entropy_contribution = proportion_school * proportion_race_school * log_ratio
      ) %>%
      summarise(theil_index = sum(entropy_contribution, na.rm = TRUE)) %>%
      pull(theil_index)
    
    return(theil_entropy_index)
  }
  
  # Create the new table with Theil index for each year-district combination
  theil_index_table <- school_data %>%
    group_by(year, District_Name) %>%
    group_modify(~ tibble(theil_entropy_index = calculate_theil_entropy_index(.x))) %>%
    ungroup()
  
  # View the resulting table
  print(theil_index_table)
  
  # Save the Theil entropy index results to a CSV file
  save_file <- paste0(single_index_folder, "theil_entropy_index_", school_type, ".csv")
  write.csv(theil_index_table, save_file, row.names = FALSE)
}

### ============================================================================
### PAIRWISE THEIL INDICES FOR RACE
### ============================================================================

for (school_type in school_types) {
  
  dta_file <- paste0("1_posted/race_", school_type, tag)  
  data <- read_dta(dta_file)
  
  data$District_Name <- as.character(as_factor(data$LAD24NM))
  # data$District_Name <- ifelse(is.na(data$District_Name),
  #                              data$LA_name, data$District_Name)
  
  school_data <- data %>%
    group_by(year, District_Name, School_Name, race) %>%
    summarise(total_students = sum(n, na.rm = TRUE)) %>%
    ungroup()
  
  # Assign racial group labels to race_data
  school_data$race_data <- ifelse(school_data$race == 1, "White",
                                  ifelse(school_data$race == 2, "Asian", "Black"))
  
  # Calculate the pairwise Theil index for each district-year combination
  calculate_pairwise_theil_index <- function(data, group_1, group_2) {
    # Filter data for the two specified races
    pairwise_data <- data %>%
      filter(race_data %in% c(group_1, group_2))
    
    # Total students for these two races at the district level
    total_students_district <- sum(pairwise_data$total_students, na.rm = TRUE)
    
    # Calculate total students per school for these two races
    total_students_school <- pairwise_data %>%
      group_by(School_Name) %>%
      summarise(school_total = sum(total_students, na.rm = TRUE))
    
    # Merge school totals with racial data
    pairwise_data <- merge(pairwise_data, total_students_school, by = "School_Name")
    
    # Calculate overall proportions for each race in the district
    race_proportions_district <- pairwise_data %>%
      group_by(race_data) %>%
      summarise(race_total = sum(total_students, na.rm = TRUE)) %>%
      mutate(race_proportion = race_total / total_students_district)
    
    # Merge race proportions with main data
    pairwise_data <- merge(pairwise_data, race_proportions_district, by = "race_data")
    
    # Calculate Theil Index
    theil_entropy_index <- pairwise_data %>%
      mutate(
        proportion_school = school_total / total_students_district,
        proportion_race_school = total_students / school_total,
        log_ratio = ifelse(proportion_race_school > 0 & race_proportion > 0,
                           log(proportion_race_school / race_proportion),
                           0),
        entropy_contribution = proportion_school * proportion_race_school * log_ratio
      ) %>%
      summarise(theil_index = sum(entropy_contribution, na.rm = TRUE)) %>%
      pull(theil_index)
    
    return(theil_entropy_index)
  }
  
  for (race_group in race_groups) {
    
    group_1 <- race_group[1]
    group_2 <- race_group[2]
    
    # Create the new table with pairwise Theil index for each year-district combination
    pairwise_theil_index_table <- school_data %>%
      group_by(year, District_Name) %>%
      group_modify(~ tibble(theil_entropy_index = calculate_pairwise_theil_index(.x, group_1, group_2))) %>%
      ungroup()
    
    # View the resulting table
    print(paste("Pairwise Theil Index for", group_1, "and", group_2))
    print(pairwise_theil_index_table)
    
    # Save the pairwise Theil entropy index results to a CSV file
    save_file <- paste0(single_index_folder, "theil_entropy_index","_",  tolower(group_1), "_", 
                        tolower(group_2), "_", school_type, ".csv")
    write.csv(pairwise_theil_index_table, save_file, row.names = FALSE)
  }
}


### ============================================================================
### CONVERT TO PANEL (POTENTIALLY EASIER TO WORK WITH)
### ============================================================================


file_list <- list.files(single_index_folder)

dissimilarity_files <- file_list[startsWith(file_list, "dissimilarity")]
theil_files <- file_list[startsWith(file_list, "theil")]

remove_csv_extension <- function(filename) {
  sub("\\.csv$", "", filename)
}

for (file in dissimilarity_files) {
  
  print(file)
  
  data <- read_csv(paste0(single_index_folder, file), col_types = cols(
    year = col_character(),
    District_Name = col_character(),
    .default = col_double()
  ))
  
  print(head(data))
  
  # Clean and reshape the data
  panel_data <- data %>%
    # Remove any leading/trailing whitespace from District_Name
    mutate(District_Name = str_trim(District_Name)) %>%
    # Fill in missing District_Name values with the previous non-missing value
    fill(District_Name) %>%
    # Pivot the data to wide format
    pivot_wider(
      names_from = year,
      values_from = dissimilarity_index,
      values_fn = mean
    ) %>%
    # Arrange rows by District_Name
    arrange(District_Name)
  
  # View the first few rows of the transformed data
  print(head(panel_data))
  
  file_name <- remove_csv_extension(file)
  
  # Optionally, save the panel data to a new CSV file
  write_csv(panel_data, paste0(panel_folder, file_name, '.csv'))
  
}

for (file in theil_files) {
  
  data <- read_csv(paste0(single_index_folder, file), col_types = cols(
    year = col_character(),
    District_Name = col_character(),
    .default = col_double()
  ))
  
  panel_data <- data %>%
    # Remove any leading/trailing whitespace from District_Name
    mutate(District_Name = str_trim(District_Name)) %>%
    # Fill in missing District_Name values with the previous non-missing value
    fill(District_Name) %>%
    # Pivot the data to wide format, summarizing duplicates by taking the mean
    pivot_wider(
      names_from = year,
      values_from = theil_entropy_index,
      values_fn = mean   # You can change 'mean' to another function like sum or first
    ) %>%
    # Arrange rows by District_Name
    arrange(District_Name)
  
  # View the first few rows of the transformed data
  print(head(panel_data))
  
  file_name <- remove_csv_extension(file)
  
  # Optionally, save the panel data to a new CSV file
  write_csv(panel_data, paste0(panel_folder, file_name, '_panel.csv'))
  
}


### ============================================================================
### CREATE COMBINED FILE WHERE YEAR-DISTRICT IDENTIFIES A ROW
### ============================================================================


full_file_name <- "full_file_cols_segregation.csv"

files_to_process <- list.files(path = single_index_folder)

df_list <- list()

for (file in files_to_process) {
  
  df <- read_csv(paste0(single_index_folder, file))
  
  file_name <- tools::file_path_sans_ext(basename(file))
  
  # Assuming the third column is the numeric value we want to keep
  # Rename it to match the file name
  names(df)[3] <- file_name
  
  # Keep only the year, District, and the renamed third column
  df <- df %>% select(year, District_Name, !!file_name)
  
  # Add the data frame to the list
  df_list[[file_name]] <- df
}

# Merge all data frames
merged_df <- df_list[[1]]  # Start with the first dataframe

# Merge with the rest of the dataframes
for (i in 2:length(df_list)) {
  merged_df <- full_join(merged_df, df_list[[i]], by = c("year", "District_Name"))
}

# Replace any remaining NAs with explicit NA
merged_df[is.na(merged_df)] <- NA

## STANDARDISE THE COLUMN NAMES ================================================

col_names <- names(merged_df)
print(names(merged_df))

rename_column <- function(col_name) {
  
  
  if (col_name == "year") {
    return(col_name)  # Keep this column as is
  } 
  if (col_name == "District_Name") {
    return("district")
  }
  
  parts <- strsplit(col_name, "_")[[1]]
  if (parts[1] == "dissimilarity") {
    first_letter <- "d"
  } else if (parts[1] == "theil") {
    first_letter <- "t"
  } else {
    return(col_name)  # If it doesn't match expected pattern, keep original name
  }
  
  if (parts[2] == "fsm") {
    category <- "fsm"
  } else if (parts[2] == "race") {
    if (parts[3] == "asian" && parts[4] == "black") {
      category <- "ab"
    } else if (parts[3] == "asian" && parts[4] == "white") {
      category <- "aw"
    } else if (parts[3] == "black" && parts[4] == "white") {
      category <- "bw"
    }
  } else if (parts[2] == "entropy") {
    if (length(parts) == 4) {  # No races specified
      category <- "race"
    } else {
      if (parts[4] == "asian" && parts[5] == "black") {
        category <- "ab"
      } else if (parts[4] == "asian" && parts[5] == "white") {
        category <- "aw"
      } else if (parts[4] == "black" && parts[5] == "white") {
        category <- "bw"
      }
    }
  } else {
    return(col_name)  # If it doesn't match expected pattern, keep original name
  }
  
  school_type <- ifelse(parts[length(parts)] == "primary", "p", "s")
  
  new_name <- paste(first_letter, category, school_type, sep="_")
  return(new_name)
}

new_names <- c()
for (name in names(merged_df)){
  rename_column(name)
}

# Apply the renaming function to all column names
new_names <- sapply(names(merged_df),function(x) {
  rename_column(x)
  })

# Rename the columns in the dataframe
names(merged_df) <- new_names

# Print the new column names
print(names(merged_df))

# Save full file as .csv
write.csv(merged_df, paste0(seg_folder,  full_file_name))

### ============================================================================
### CREATE COMBINED FILE WHERE INDEX-DISTRICT IDENTIFIES A ROW
### ============================================================================

full_file_name <- "full_file_cols_years.csv"

# Get list of files
file_list <- list.files(path = panel_folder, full.names = TRUE)

# Function to extract index type and school level from filename
get_index_info <- function(filename) {
  parts <- strsplit(basename(filename), "_")[[1]]
  if (parts[1] == "dissimilarity") {
    first_letter <- "d"
    if (parts[2] == "fsm") {
      cat <- "fsm"
    } else if (parts[2] == "race") {
      if (parts[3] == "asian" && parts[4] == "black") {
        cat <- "ab"
      } else if (parts[3] == "asian" && parts[4] == "white") {
        cat <- "aw"
      } else if (parts[3] == "black" && parts[4] == "white") {
        cat <- "bw"
      }
    }
  } else if (parts[1] == "theil") {
    first_letter <- "t"
    if (parts[4] == "asian" && parts[5] == "black") {
      cat <- "ab"
    } else if (parts[4] == "asian" && parts[5] == "white") {
      cat <- "aw"
    } else if (parts[4] == "black" && parts[5] == "white") {
      cat <- "bw"
    } else {
      cat <- "all"
    }
  }
  school_type <- ifelse(grepl("primary", filename), "p", "s")
  return(paste(first_letter, cat, school_type, sep="_"))
}

# Initialize an empty list to store data frames
df_list <- list()

# Loop through each file
for (file in file_list) {
  # Read the CSV file
  df <- read_csv(file)
  
  # Get index info
  index_info <- get_index_info(file)
  
  # Replace spaces with underscores in District_Name and add Index column
  df <- df %>%
    mutate(district = str_replace_all(District_Name, " ", "_"),
           index = index_info)
  
  # Add to list
  df_list[[index_info]] <- df
}

# Combine all data frames
combined_df <- bind_rows(df_list)
combined_df <- combined_df %>%
  select(index, district, everything()) %>%
  select(-District_Name) 

# Save the reshaped data
write.csv(combined_df, paste0(seg_folder, full_file_name))


### ============================================================================
### END OF FILE
### ============================================================================
