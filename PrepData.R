
setwd("./segDataPrep/")

source("./03_data_cleaning_post_08.R")
source("/04_link_to_LADs.R")
source("/05_generate_segplots.R")
source("/06_generate_segregation_indices.R")
source("/07_generate_shapefile_for_app.R")


# Switch back to the repository home directory
setwd('../')
files_to_move <- list.files('./segDataPrep/outputs')

for (file in files_to_move){
  file_name <- basename(file)
  file_path <- paste0('segDataPrep/outputs/', file_name)
  new_file_path <- paste0('app/src/data/', file_name)
  new_file_path <- gsub('geojson', 'json', new_file_path)
  file.copy(file_path, new_file_path, overwrite = T)
}

