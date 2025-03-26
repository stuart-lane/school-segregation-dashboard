

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


# Define file path for raw data files
raw_data_dir <- "5_DfE data/Schools Pupils and their Characteristics/"

# Define location for temporary .csv files and final .dta files
tmp_dir <- "99_tmp/"
final_dir <- "1_posted/"

# Import list of raw file paths
source("raw_file_paths.R")


### ============================================================================
### ============================================================================
### PRODUCE TEMPORARY .CSV FILES BEFORE
### ============================================================================
### ============================================================================


for (raw_file in raw_files) {

  ## DATA LOADING AND BASIC FORMATTING -----------------------------------------
  file_path <- file.path(paste0(raw_data_dir, raw_file))


  # Determine if intitial blank rows must be skipped
  if (!startsWith(raw_file, "2002_09")) {
    data <- fread(file_path)
  } else {
    skip_rows <- get_skip_rows(raw_file)
    data <- fread(file_path, skip = skip_rows, header = TRUE)
  }

  # Re-format fataset as a dataframe
  data <- as.data.frame(data)

  ## SELECT AND RENAME COLUMNS -------------------------------------------------

  data <- data %>%
    select(starts_with("region"), starts_with("urn"), starts_with("la"),
           starts_with("old"), starts_with("new"), starts_with("school"),
           starts_with("phase"), starts_with("type"), starts_with("denom"),
           starts_with("district"), starts_with("headcount of pupils"),
           starts_with("ward"), starts_with("number of fsm eligible"),
           starts_with("number of pupils known to be eligible f"),
           starts_with("number of pupils used for fsm calculat"),
           starts_with("number of pupils classified"), starts_with("urban"),
           starts_with("academy"), starts_with("estab"), matches("^(?i)urn"),
           starts_with("form 7"))

  data <- data %>%
    rename(
      whiteb = `number of pupils classified as white British ethnic origin`,
      irishe = `number of pupils classified as Irish ethnic origin`,
      travel = `number of pupils classified as traveller of Irish heritage ethnic origin`,
      anyoth = `number of pupils classified as any other white background ethnic origin`,
      gypsyr = `number of pupils classified as Gypsy/Roma ethnic origin`,
      whitea = `number of pupils classified as white and black Caribbean ethnic origin`,
      indian = `number of pupils classified as Indian ethnic origin`,
      pakist = `number of pupils classified as Pakistani ethnic origin`,
      bangla = `number of pupils classified as Bangladeshi ethnic origin`,
      chines = `number of pupils classified as Chinese ethnic origin`
    )

  ## DROP UNUSED COLUMNS -------------------------------------------------------

  columns_to_drop <- c(
    "number of pupils classified as white and black African ethnic origin",
    "number of pupils classified as white and Asian ethnic origin",
    "number of pupils classified as any other mixed background ethnic origin",
    "number of pupils classified as any other black background ethnic origin",
    "number of pupils classified as any other Asian background ethnic origin",
    "number of pupils classified as any other ethnic group ethnic origin"
  )

  data <- data %>%
    select(-any_of(columns_to_drop))

  ## RENAME COLUMNS WITH WIDER VARIATION IN NAMES ------------------------------

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("region_name", ., ignore.case = TRUE) ~ "Region",
        grepl("region_code", ., ignore.case = TRUE) ~ "Region code",
        grepl("la_name", ., ignore.case = TRUE) ~ "LA name",
        grepl("new_la_name", ., ignore.case = TRUE) ~ "new_LA_name",
        grepl("new_la_code", ., ignore.case = TRUE) ~ "new_LA_code",
        grepl("school_name", ., ignore.case = TRUE) ~ "School Name",
        grepl("school_postcode", ., ignore.case = TRUE) ~ "School Postcode",
        grepl("school_code", ., ignore.case = TRUE) ~ "School Code",
        grepl("phase_type_grouping", ., ignore.case = TRUE) ~ "Phase-type grouping",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("pupils taking.*free school meal",
              ., ignore.case = TRUE) ~ "fsm_taking",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("pupils known to be eligible for.*free school meals", .,
              ignore.case = TRUE) &
          !grepl("Performance Tables|AATs|SCAAT|PAT", .,
                 ignore.case = TRUE) ~ "fsm_eligible",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("pupils known to be eligible for free school meals.*(Performance Tables|AATs|SCAAT|PAT)", ., ignore.case = TRUE) ~ "fsm_total",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("count of pupils", ., ignore.case = TRUE) ~ "headcount",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("African ethnic origin", ., ignore.case = TRUE) ~ "africa",
        TRUE ~ .
      ),
      .cols = everything()
    )

  data <- data %>%
    rename_with(
      ~ case_when(
        grepl("Caribbean ethnic origin", ., ignore.case = TRUE) ~ "caribb",
        TRUE ~ .
      ),
      .cols = everything()
    )

  ## REPLACE VALUES AND CONVERT TO NUMERIC -------------------------------------

  data <- data %>%
    mutate(across(c(whiteb, irishe, travel, anyoth, gypsyr, whitea, indian,
                    pakist, bangla, caribb, africa, chines, starts_with("fsm")),
                  ~ replace(., . == "z", NA)))

  data <- data %>%
    mutate(across(c(headcount, whiteb, irishe, travel, anyoth, gypsyr,
                    whitea, indian, pakist, bangla, caribb, africa,
                    chines, starts_with("fsm")),
                  as.numeric))

  ## DROP FURTHER COLUMNS FOUND ONLY IN SOME DATASETS

  cols_to_remove <- names(data)[grepl("\\(PRUs only\\)", names(data))]

  # Print columns that will be removed
  cols_to_remove <- names(data)[grepl("\\(PRUs only\\)|ethnic", names(data),
                                      ignore.case = TRUE)]
  print(paste("Columns being removed:", paste(cols_to_remove, collapse = ", ")))

  # Remove the columns
  data <- data %>%
    select(-matches("\\(PRUs only\\)|ethnic", ignore.case = TRUE))


  ## EXTRACT YEAR STRING AND SAVE OUTPUT FILE ----------------------------------

  if (!startsWith(raw_file, "2002_09")) {
    year_string <- substr(raw_file, 1, 7)
  } else {
    year_string <- extract_school_year(raw_file)
  }

  data$year <- year_string

  # Save cleaned data to a new file as CSV
  output_file <- file.path(tmp_dir, paste0("schooldata_", year_string, ".csv"))
  write.csv(data, file = output_file, row.names = FALSE)

}


### ============================================================================
### ============================================================================
### COMBINE TEMPORARY FILES
### ============================================================================
### ============================================================================


# List files generated in previous code block
files <- list.files(tmp_dir, pattern = "schooldata_.*\\.csv$",
                    full.names = TRUE)

## ENSURE HARMONISED DATA TYPES AND COLUMN NAMES -------------------------------

column_harmonization <- list(
  "District Administrative code" = c("District Administrative code",
                                     "district_administrative_code"),
  "District Administrative name" = c("District Administrative name",
                                     "district_administrative_name"),
  "LA number" = c("LA number", "la number", "LA number -LGR 2009"),
  "LA name" = c("LA name", "la name", "LA name - LGR 2009"),
  "School Name" = c("School Name", "School name", "school name"),
  "School Type" = c("School Type", "School type", "school type"),
  "Type of Establishment" = c("TypeOfEstablishment (name)",
                              "type_of_establishment",
                              "typeofestablishment_name",
                              "TypeOfEstablishment",
                              "Type of establishment"),
  "Urban Rural" = c("Urban / Rural", "Urban rural", "urban_rural"),
  "Region" = c("Region"),
  "Region code" = c("Region Code", "Region code"),
  "old_LA_code" = c("old_LA_code", "Old_LA_code", "old_la_code"),
  "new_LA_code" = c("new_LA_code"),
  "Phase-type grouping" = c("Phase-type grouping", "phase-type_grouping",
                            "Phase of education"),
  "Ward code" = c("Ward code", "ward_code"),
  "Ward name" = c("Ward name", "ward_name"),
  "School Postcode" = c("School Postcode"),
  "School size" = c("School size", "school_size"),
  "Academy flag" = c("Academy_flag", "academy_flag"),
  "LAESTAB" = c("LAESTAB", "LAEstab", "laestab", "LAESTAB no.")
)

harmonize_columns <- function(data) {
  for (standard_name in names(column_harmonization)) {
    variations <- column_harmonization[[standard_name]]
    for (var in variations) {
      if (var %in% names(data)) {
        data <- data %>% rename_with(~standard_name, all_of(var))
        break  # Stop after first match
      }
    }
  }
  return(data)
}

numeric_columns <- c("headcount", "fsm_taking", "fsm_eligible", "fsm_total",
                     "whiteb", "irishe", "travel", "anyoth", "gypsyr", "whitea",
                     "indian", "pakist", "bangla", "caribb", "africa", "chines")

character_columns <- c("urn", "LAESTAB", "estab", "School Type", "Urban Rural",
                       "Ward name", "Type of Establishment")

## DEFINE FUNCTION TO READ .CSV FILES AND IMPLEMENT HARMONISATION --------------

read_csv_file <- function(file_path) {
  # Read the CSV file as character
  data <- read_csv(file_path, col_types = cols(.default = col_character()))

  # Harmonize column names
  data <- harmonize_columns(data)

  # Attempt to convert types
  data <- type_convert(data)

  # Force numeric columns (only for columns that exist)
  data <- data %>%
    mutate(across(any_of(numeric_columns), ~as.numeric(gsub("[^0-9.-]",
                                                            "", .))))

  # Force character columns
  data <- data %>%
    mutate(across(any_of(character_columns), as.character))

  # Extract year from file name and add it as a column
  year <- sub(".*schooldata_(.+)\\.csv$", "\\1", basename(file_path))
  data$year <- year

  return(data)
}

## CREATE COMBINED .CSV FILE AND SAVE OUTPUT -----------------------------------

combined_data <- map_dfr(files, read_csv_file)

# Sort the final dataset by year
combined_data <- combined_data %>%
  arrange(year)

# Print the dimensions of the combined dataset
print(dim(combined_data))

# Optionally, save the combined dataset
write_csv(combined_data, file.path(tmp_dir, "combined_schooldata.csv"))


### ============================================================================
### ============================================================================
### FURTHER DATA CLEANING
### ============================================================================
### ============================================================================


combined_file_name <- "combined_schooldata.csv"

file_path <- file.path(paste0(tmp_dir, combined_file_name))

data <- read_csv(file_path)

colSums(is.na(data)) 

colnames(data)



# Sort by year
data <- data %>% arrange(year)

# Display full description (equivalent to des, full in Stata)
str(data)
summary(data)

# Replace region name
data <- data %>%
  mutate(Region = ifelse(Region == "Yorkshire and The Humber",
                         "Yorkshire and the Humber", Region))

# Replace district name and encode
data <- data %>%
  mutate(`District Administrative name` = ifelse(
    `District Administrative name` == "Unknown", NA,
    `District Administrative name`),
         District = as.factor(`District Administrative name`)) %>%
  mutate(District = fct_recode(District, NULL = "Unknown")) %>%
  select(-`District Administrative name`) %>%
  rename(`District Code` = `District Administrative code`)

# Replace LA names
data <- data %>%
  mutate(
    `LA name` = case_when(
      `LA name` == "Bristol City of" ~ "Bristol, City of",
      `LA name` == "Bournemouth, Christchurch and Poole Council" ~ "Bournemouth, Christchurch and Poole",
      `LA name` == "Herefordshire, County of" ~ "Herefordshire",
      `LA name` == "Isles Of Scilly" ~ "Isles of Scilly",
      `LA name` == "Kingston upon Hull City of" ~ "Kingston upon Hull, City of",
      TRUE ~ `LA name`
    )
  )


### THIS IS WHERE TROUBLE HAPPENS IN THE CODE, WE NEED TO MAKE SURE THAT THIS
### CAPTURES THE DIFFERENCES BETWEEN THE YEARS SO THAT WE CAN CONTINUE TO GET ALL YEARS INCLUDED

# Optionally, save the combined dataset
# write_csv(data, file.path(tmp_dir, "combined_schooldata_check.csv"))


# Phase/type groupings will change a lot over the years

# Extract the final two characters of each year string
final_two_digits <- substr(data$year, nchar(data$year) - 1, nchar(data$year))
data$year_num <- as.numeric(final_two_digits)

data$`Phase-type grouping`[
  (data$year %in% c("2007_08", "2008_09")) & 
    (data$`School Type` %in% c("Community School", "Free Schools", "Voluntary Aided School", "Voluntary Controlled School")) &
    (data$`Phase-type grouping` == "Primary")
] <- "State-funded primary"

data$`Phase-type grouping`[
  (data$year %in% c("2007_08", "2008_09")) & 
    (data$`School Type` %in% c("Community School", "Free Schools", "Voluntary Aided School", "Voluntary Controlled School")) &
    (data$`Phase-type grouping` == "Secondary")
] <- "State-funded secondary"

data <- data %>%
  mutate(
    # First condition for year 2014 and beyond
    `Phase-type grouping` = ifelse(
      year_num >= 14 & `Phase-type grouping` == "State-funded AP school",
      "Pupil referral unit",
      `Phase-type grouping`
    ),

    # Apply conditions for years 2010-2013
    `Phase-type grouping` = case_when(
      year_num >= 11 & year_num <= 13 & `Phase-type grouping` == "Primary" &
        `Type of Establishment` %in% c("Community School", "Free Schools",
                                       "Voluntary Aided School",
                                       "Voluntary Controlled School") ~ "State-funded primary",

      # was between 10 and 13
      year_num >= 11 & year_num <= 13 & `Phase-type grouping` == "Secondary" &
        `Type of Establishment` %in% c("Community School", "Free Schools",
                                       "Voluntary Aided School",
                                       "Voluntary Controlled School") ~ "State-funded secondary",

      # was just 9
      year_num >= 9 & year_num <= 10 & `Phase-type grouping` == "Primary" ~ "State-funded primary",

      year_num >= 9 & year_num <= 10 & `Phase-type grouping` == "Secondary" ~ "State-funded secondary",

      TRUE ~ `Phase-type grouping`
    )
  )

# write_csv(data, file.path(tmp_dir, "combined_schooldata_check07-09.csv"))

# If you want to remove `Phase-type grouping` after the transformations
data <- data %>%
  mutate(type = as.factor(`Phase-type grouping`)) %>%
  select(-`Phase-type grouping`)


# Set race and FSM to missing for private schools
vars_to_set_missing <- c("whiteb", "irishe", "travel", "anyoth", "gypsyr",
                         "whitea", "indian", "pakist", "bangla", "caribb",
                         "africa", "chines", "fsm_eligible", "fsm_taking",
                         "fsm_total")

data <- data %>%
  mutate(across(all_of(vars_to_set_missing), ~ifelse(type == 1, NA, .)))

# Function to safely sum columns that exist
safe_sum <- function(df, cols) {
  existing_cols <- intersect(names(df), cols)
  if (length(existing_cols) > 0) {
    rowSums(df[existing_cols], na.rm = TRUE)
  } else {
    rep(0, nrow(df))
  }
}

# Aggregate racial categories
data <- data %>%
  mutate(
    asian = safe_sum(., c("indian", "pakist", "bangla", "chines")),
    black = safe_sum(., c("caribb", "africa", "whitea")),
    white = safe_sum(., c("whiteb", "irishe", "travel", "anyoth", "gypsyr")),
    totrace = asian + black + white
  )

# Race percentages
data <- data %>%
  mutate(
    perc_asian = asian / totrace * 100,
    perc_black = black / totrace * 100,
    perc_white = white / totrace * 100
  )

data <- data %>%
  mutate(across(starts_with("perc_"), ~round(., 1)))


### ============================================================================
### ============================================================================
### FREE SCHOOL MEAL DATA CLEANING
### ============================================================================
### ============================================================================


# Free school meals variables
data <- data %>%
  mutate(
    fsm_non_eligible = headcount - fsm_eligible,
    perc_fsm_elig = fsm_eligible / headcount * 100,
    fsm_non_taking = headcount - fsm_taking,
    perc_fsm_taking = fsm_taking / headcount * 100
  )

# Reorder columns
data <- data %>%
  select(
    Region, `Region code`, District, `District Code`, `Ward name`, `Ward code`,
    type, `School Name`, year, Denomination, headcount,
    starts_with("fsm"), everything()
  )

# Sort the data
data <- data %>%
  arrange(`Region code`, `District Code`, `Ward code`, type, `School Name`, year)



### ============================================================================
### ============================================================================
### CREATE FINAL FILES
### ============================================================================
### ============================================================================




## CREATE RACE DATASETS --------------------------------------------------------

# (REMOVE STARTS WITH `LA NAME` TO OBTAIN THE OLD DATA FILES)

# Filter the data
data_race <- data %>%
  filter(type %in% c("State-funded primary", "State-funded secondary")) %>%
  select(white, asian, black, totrace, headcount,starts_with("perc"), starts_with("LA name"),
         `School Name`, type, starts_with("Region"), starts_with("District"), starts_with("LAD24"),
         starts_with("Ward"), year, urn)

# Reshape the data from wide to long format
data_long <- data_race %>%
  mutate(tmpid = row_number()) %>%
  pivot_longer(cols = c(white, asian, black),
               names_to = "race",
               values_to = "n") %>%
  mutate(race = case_when(
    race == "white" ~ 1,
    race == "asian" ~ 2,
    race == "black" ~ 3
  )) %>%
  arrange(`School Name`, year)




# Create factor for race
data_long$race <- factor(data_long$race,
                         levels = 1:3,
                         labels = c("White", "Asian", "Black"))

data_long <- data_long %>%
  rename_all(~str_replace_all(., "\\s", "_"))

# Split and save primary schools data
primary_long <- data_long %>%
  filter(type == "State-funded primary")
write_dta(primary_long, file.path(final_dir, "race_primary_long.dta"))

# Split and save secondary schools data
secondary_long <- data_long %>%
  filter(type == "State-funded secondary")
write_dta(secondary_long, file.path(final_dir, "race_secondary_long.dta"))

## CREATE FSM DATASETS --------------------------------------------------------

# Filter the data for State-funded primary and secondary schools
data_fsm <- data %>%
  filter(type %in% c("State-funded primary", "State-funded secondary")) %>%
  select(starts_with("fsm"), headcount, starts_with("perc"), starts_with("LA name"),starts_with("LAD24"),
         `School Name`, type, starts_with("Region"), starts_with("District"),
         starts_with("Ward"), year, urn)

# Generate the n1 and n2 variables
data_fsm <- data_fsm %>%
  mutate(n1 = fsm_eligible,
         n2 = fsm_non_eligible) %>%
  select(-starts_with("fsm")) %>%
  mutate(tmpid = row_number())

# Reshape the data from wide to long format
data_long <- data_fsm %>%
  pivot_longer(cols = c(n1, n2),
               names_to = "fsm",
               values_to = "n") %>%
  mutate(fsm = case_when(
    fsm == "n1" ~ 1,
    fsm == "n2" ~ 2
  )) %>%
  arrange(`School Name`, year, fsm)

# Create factor for fsm
data_long$fsm <- factor(data_long$fsm,
                        levels = c(1, 2),
                        labels = c("FSM eligible", "Not FSM eligible"))

# Reorder the columns to match the order in Stata
data_long <- data_long %>%
  select(`School Name`, year, fsm, n, everything())

data_long <- data_long %>%
  rename_all(~str_replace_all(., "\\s", "_"))

# Split and save primary schools data
primary_long <- data_long %>%
  filter(type == "State-funded primary") # "State-funded primary"
write_dta(primary_long, file.path(final_dir, "fsm_primary_long.dta"))

# Split and save secondary schools data
secondary_long <- data_long %>%
  filter(type == "State-funded secondary") # "State-funded secondary"
write_dta(secondary_long, file.path(final_dir, "fsm_secondary_long.dta"))


### ============================================================================
### ============================================================================
### END OF FILE
### ============================================================================
### ============================================================================


# LEFT TO DO - PRE 2008-09 files
# with these files, i will need to play around with the form 7 school types -
# this is for another day though!

# This code is just to load the data and see where the current years are ending

check_data <- read_dta(paste0(tmp_dir, "fsm_primary_long.dta"))

print(unique(check_data$year))

print(dim(check_data))

print(names(check_data))
