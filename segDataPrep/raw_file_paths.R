### ============================================================================
### BLOCK 1
### ============================================================================

# Define file paths as R strings
f2023_24 <- "2023_24/supporting-files/spc_school_level_underlying_data.csv"
f2022_23 <- "2022_23/supporting-files/spc_school_level_underlying_data_23112023.csv"
f2021_22 <- "2021_22/supporting-files/spc_school_level_underlying_data_20230302.csv"
f2020_21 <- "2020_21/supporting-files/spc_school_level_underlying_data_220216.csv"
f2019_20 <- "2019_20/supporting-files/spc_school_level_underlying_data.csv"

year_strings_2019_2024 <- c("2023_24", "2022_23", "2021_22", "2020_21", "2019_20")
raw_files_2019_2024 <- c(f2023_24, f2022_23, f2021_22, f2020_21, f2019_20)

### ============================================================================
### BLOCK 2
### ============================================================================

f2018_19 <- "2018_19/Schools_Pupils_and_their_Characteristics_2019_pupil_characteristics_UD.csv"
f2017_18 <- "2017_18/Schools_Pupils_and_their_Characteristics_2018_Schools_Pupils_UD.csv"
f2016_17 <- "2016_17/SFR28_2017_Schools_Pupils_UD.csv"
f2015_16 <- "2015_16/SFR20_2016_Schools_Pupils_UD.csv"
f2014_15 <- "2014_15/SFR16_2015_Schools_Pupils_UD.csv"
f2013_14 <- "2013_14/SFR15_2014_school_level_pupils_UD.csv"
f2012_13 <- "2012_13/School_level_schools_pupil_2013.csv"
f2011_12 <- "2011_12/School_level_schools_pupils_2012.csv"
f2010_11 <- "2010_11/School_level_schools_pupils_2011.csv"
f2009_10 <- "2009_10/school_level_census.csv"

year_strings_2009_2019 <- c("2018_19", "2017_18", "2016_17", "2015_16", "2014_15",
                            "2013_14", "2012_13", "2011_12", "2010_11", "2009_10")
raw_files_2009_2019 <- c(f2018_19, f2017_18, f2016_17, f2015_16, f2014_15,
                         f2013_14, f2012_13, f2011_12, f2010_11, f2009_10)

### ============================================================================
### BLOCK 3
### ============================================================================

f2008_09 <- "2002_09/schoolfinal09#.csv"
f2007_08 <- "2002_09/schoolfinal08#.csv"
f2006_07 <- "2002_09/schoolfinal07#.csv"
f2005_06 <- "2002_09/schoolfinal06#.csv"
f2004_05 <- "2002_09/schoolfinal05#.csv"
f2003_04 <- "2002_09/schoolfinal04#.csv"
f2002_03 <- "2002_09/schoolfinal03#.csv"
# f2001_02 <- "2002_09/schoolfinal02#.csv"


year_strings_2002_2009 <- c("2008_09", "2007_08", "2006_07", "2005_06",
                            "2004_05", "2003_04", "2002_03")
raw_files_2002_2009 <- c(f2008_09, f2007_08, f2006_07, f2005_06,
                         f2004_05, f2003_04, f2002_03)

### ============================================================================
### OVERALL
### ============================================================================

year_strings <- c(year_strings_2019_2024, year_strings_2009_2019, year_strings_2002_2009)
raw_files <- c(raw_files_2019_2024, raw_files_2009_2019, raw_files_2002_2009)

### ============================================================================
### FUNCTION TO SKIP ROWS IN 2002-2009 .CSV FILES
### ============================================================================

get_skip_rows <- function(file_path) {
  char_21 <- substr(file_path, 21, 21)

  if (char_21 %in% c("9", "8")) {
    return(5)
  } else if (char_21 == "7") {
    return(2)
  } else if (char_21 == "6") {
    return(1)
  } else if (char_21 %in% c("5", "4", "3", "2")) {
    return(4)
  } else {
    stop("Unexpected file name pattern")
  }
}

### ============================================================================
### FUNCTION TO CREATE FILE NAMES TAGS FOR THE 2002_09 DATASETS
### ============================================================================

extract_school_year <- function(raw_file) {

  # Extract the two-digit year from the filename
  year <- sub(".*schoolfinal(\\d{2})#\\.csv", "\\1", raw_file)

  # Convert to integer and add 2000 to get the full year
  full_year <- as.integer(year) + 2000

  # Construct the desired string format
  sprintf("%d_%02d", full_year - 1, as.integer(substr(full_year, 3, 4)))
}

### ============================================================================
### SET TYPES OF COLUMNS
### ============================================================================

col_types <- cols(
  Region = col_character(),
  `Region code` = col_character(),
  `LA name` = col_character(),
  new_LA_code = col_character(),
  `School Name` = col_character(),
  `School Postcode` = col_character(),
  `School size` = col_character(),
  `Phase-type grouping` = col_character(),
  URN = col_double(),
  LAESTAB = col_double(),
  `old_LA_code` = col_double(),
  headcount = col_double(),
  fsm_taking = col_double(),
  fsm_eligible = col_double(),
  fsm_total = col_double(),
  whiteb = col_double(),
  irishe = col_double(),
  travel = col_double(),
  anyoth = col_double(),
  gypsyr = col_double(),
  whitea = col_double(),
  indian = col_double(),
  pakist = col_double(),
  bangla = col_double(),
  caribb = col_double(),
  africa = col_double(),
  chines = col_double()
  )


