library(sf)
library(dplyr)
library(tidyr)
library(viridis)
library(cowplot)
library(ggplot2)

prepare_spatial_data <- function(shape_file_directory){
  
  spatial_df <- sf::read_sf(shape_file_directory)
  
  # Simplify the shapefile to reduce memory
  # spatial_df <- rmapshaper::ms_simplify(spatial_df, keep = 0.001,keep_shapes = FALSE)
  # Transform into lat-long projection
  spatial_df <- sf::st_transform(spatial_df, 4326)
  
  return(spatial_df)
  
}


get_plot_information <- function(figure_directory){
  
  years_folders <- list.files(figure_directory)
  plot_types <- lapply(years_folders, function(year_folder){
    list.files(paste0(figure_directory, year_folder,collapse = '/'))
  }) |> unlist() |> unique()
  
  all_plots <- list()
  
  i <- 1
  for (year_folder in years_folders){
    for (plot_type in plot_types){
      
      dir_to_check <- paste0(figure_directory, '/',year_folder,'/' ,plot_type,'/')
      dir_to_check <- gsub('//', '/', dir_to_check)
      
      dir_plots <- list.files(dir_to_check)
      
      
      file_suffix <- paste0('_', plot_type, '_', year_folder,'.png')
      
      for (plot in dir_plots){
        
        area <- gsub(file_suffix, '', plot)
        if(area==''){
          next
        }
        
        plot_path <- paste0(
          dir_to_check,
          plot
        )
        
        all_plots[[i]] <- list(
          year_store = year_folder,
          plot_type_store = plot_type,
          areas_store = area,
          plot_path = plot_path
        )
        
        i <- i + 1
      }
      
    }
  }
  
  
  all_plots <- dplyr::bind_rows(all_plots)
  
  all_plots$year <- substr(all_plots$year_store, 1, 4)
  
  
  to_title <- function(x) {
    x <- gsub("(^|_)([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
    x <- gsub("(^|-)([[:alpha:]])", "\\1\\U\\2", x, perl = TRUE)
    
    x <- gsub("_", " ", x)
    
    x <- gsub(" Of ", " of ", x)
    x <- gsub(" On ", " on ", x)
    x <- gsub(" And ", " and ", x)
    x <- gsub(" With ", " with ", x)
    x <- gsub(" In ", " in ", x)
    x <- gsub("-In-", "-in-", x)
    x <- gsub(" County Of", " County of", x)
    x <- gsub(" City Of", " City of", x)
    
    return(x)
    
  }
  
  
  all_plots$area <- to_title(all_plots$areas_store)
  
  all_plots$plot_path <- gsub('./', '/', all_plots$plot_path,fixed=T)
  
  return(all_plots)
  
}

link_seg_indices <- function(spatial_data, seg_indice_directory, plot_information, legends_output_directory){

   seg_indice_directory <- './3_segregation_indices/'
   indice_data <- readr::read_csv(paste0(seg_indice_directory, 'full_file_cols_segregation.csv'))
  # 
  indice_data$link <- tolower(indice_data$district)
  indice_data$link <- gsub(' ','_', indice_data$link)
  indice_data$link <- gsub('-','_', indice_data$link)
  indice_data$link <- gsub(',','_', indice_data$link)
  
  grouping <- list(
    "fsm" = "fsm",
    "race" = 'race'
    # "Asian/Black" = "ab",
    # "Asian/White" = "aw",
    # "Black/White" = "bw"
  )
  
  schools <- list(
    "primary" = "p",
    "secondary" = "s" 
  )
  
  filters <- list(
    # index = type_of_index,
    group = grouping,
    school = schools
  )
  
  seg_info <- list()
  
  combos <- expand.grid(unique(indice_data$link), unique(indice_data$year), unique(names(filters$group)), unique(names(filters$school)))
  combos <- combos[complete.cases(combos),]
  colnames(combos) <- c('link', 'year', 'group', 'school')
  combos$year <- as.character(combos$year)
  
  
  seg_index_info <- list()
  
  
  for (row in 1:nrow(combos)){
    
    # color <- ifelse()
    
    # index <- filters[['index']][[combos$index[row]]]
    group <- filters[['group']][[combos$group[row]]]
    
    if (group == 'fsm'){
      index <- 'd'
    } else {
      index <- 't'
    }
    
    school <- filters[['school']][[combos$school[row]]]
    
    
    column <- paste0(index, "_", group, '_', school)
    
    # which(indice_data$link == combos$link[row])
    # which(indice_data$year == combos$year[row])
    value <- indice_data[indice_data$link == as.character(combos$link[row]) & indice_data$year == combos$year[row] & !is.na(indice_data$district), column]
    if (nrow(value)==1){
      value <- as.numeric(value)
    }else if(nrow(value) > 1 & sum(is.na(value)) ==1){
      value <- value[!is.na(value)]
      value <- as.numeric(value)
    } else if (nrow(value)==0){
      value <- NA
      warning(paste0("No value for the index: ", row))
      
    }else {
      
      warning(paste0("Multiple values for the same index: ", row))
      value <- NA
      
    }
    
    combos$year[row] <- substr(as.character(combos$year[row]), 1, 4)
    
    seg_index_info[[row]] <- list(
      'link' = combos$link[row],
      'year' = combos$year[row],
      'index' = combos$index[row],
      'group' = combos$group[row],
      'school' = combos$school[row],
      'value' = value
    )
  }
  
  seg_index_info <- seg_index_info |> dplyr::bind_rows()
  seg_index_info <- create_color_bins(seg_index_info, legends_output_directory)
  
  return(seg_index_info)
  
}


create_color_bins <- function(seg_index_info, legends_output_directory){
  seg_index_info_summary <- seg_index_info |> 
    dplyr::group_by(year,
                    group,
                    school) |> 
    summarise(min = min(value, na.rm = T),
              max = max(value, na.rm = T)) 
  
  seg_index_info$colour <- NA
  seg_index_info$break_label <- NA
  for (row in 1:nrow(seg_index_info_summary)){
    
    year <- seg_index_info_summary$year[row]
    group <- seg_index_info_summary$group[row]
    school <- seg_index_info_summary$school[row]
    

    min_value <- seg_index_info_summary$min[seg_index_info_summary$year == year & seg_index_info_summary$group == group & seg_index_info_summary$school == school]
    max_value <- seg_index_info_summary$max[seg_index_info_summary$year == year & seg_index_info_summary$group == group & seg_index_info_summary$school == school]
    
    
    subset <- seg_index_info$year == year & seg_index_info$group == group & seg_index_info$school == school
    breaks <- seq(min_value, max_value, length.out = 6)
    break_labels <- c()
    for (i in 1:(length(breaks)-1)){
      break_labels[i] <- paste0(signif(breaks[i],2), ' - ', signif(breaks[i+1],2))
    }
    break_labels <- c(break_labels, 'NA')
    
    if (length(unique(breaks))==6){
    colors <- viridis::viridis(5)
    seg_index_info$colour[subset] <- cut(
      seg_index_info$value[subset],
      breaks = breaks,
      labels = colors
    )}else{
      colors <- viridis::viridis(1)
      seg_index_info$colour[subset][!is.na(seg_index_info$value[subset])] <- "1"
      break_labels <- c(unique(signif(breaks,2)), "NA")
    }
    
    for (i in 1:length(seg_index_info$colour[subset])){
      
      value <- as.numeric(seg_index_info$colour[subset][i])
      if (!is.na(value)){
      seg_index_info$colour[subset][i] <- colors[value]
      seg_index_info$break_label[subset][i] <- break_labels[value]
      }else{
        seg_index_info$colour[subset][i] <- "#808080"
        seg_index_info$break_label[subset][i] <- 'NA'
      }
    }
    
    temp <- seg_index_info[subset,c("value", 'colour', 'break_label')]
    temp$break_label <- factor(temp$break_label, levels = break_labels, ordered = T)
    
    
    temp_plot <- ggplot(temp, aes(x = break_label, fill=break_label)) +
      geom_bar() +
      scale_fill_manual(values = c(colors,"#808080"), labels = break_labels) +
      labs(fill='Thresholds')
    
    legend <- cowplot::get_legend(temp_plot)
    
    output_path <- paste0(legends_output_directory, year, '_', group, '_', school, '.svg')
    output_path <- gsub("  ", " ", output_path)
    output_path <- gsub(" ", "_", output_path)
    ggsave(output_path, 
           legend,
           width = 300,
           height = 500,
           unit='px')
    
  }
  
  
  return(seg_index_info)
  

  
}

prepare_shapefile <- function(shape_file_directory,
                              figure_directory,
                              output_directory){
  
 
  spatial_data <- prepare_spatial_data(shape_file_directory)
  spatial_data <-  spatial_data[substr(spatial_data$LAD24CD, 1, 1) == 'E',]
  plot_information <- get_plot_information(figure_directory)
  
  
  

  
  spatial_data$link <- tolower(spatial_data$LAD24NM)
  spatial_data$link <- gsub(' ','_', spatial_data$link)
  spatial_data$link <- gsub('-','_', spatial_data$link)
  spatial_data$link <- gsub(',','_', spatial_data$link)
  
  
  plot_information$link <- tolower(plot_information$areas_store)
  plot_information$link <- gsub(' ','_', plot_information$link)
  plot_information$link <- gsub('-','_', plot_information$link)
  plot_information$link <- gsub(',','_', plot_information$link)
  
  
  missing_areas <- length(unique(plot_information$link[plot_information$link %in% spatial_data$link==F]))
  if(length(missing_areas)>0){
    warning(paste0('Missing areas: ', missing_areas))
  }
  
  area_list <- list()
  for (i in 1:nrow(plot_information)){
    area_list[[i]] <- list(
      'name'=plot_information$area[i],
      'link'=plot_information$link[i])
  }
  
  
  
  area_list <- unique(area_list)
  
  filters <- list(
    'year' = sort(unique(plot_information$year)),
    'plot_type' = sort(unique(plot_information$plot_type_store)),
    'area' = area_list)
  
  dir.create(output_directory, showWarnings = F)
  dir.create(paste0(output_directory, 'legends/'), showWarnings = F)
  
  seg_indices <- link_seg_indices(spatial_data, 
                                  seg_indice_directory, 
                                  plot_information, 
                                  legends_output_directory = paste0(output_directory, "legends/"))
  
  

  seg_indices <- jsonlite::toJSON(seg_indices, pretty = T,auto_unbox = T)
  readr::write_file(seg_indices, paste0(output_directory, 'seg_indices.json'))
                    
  plot_information <- jsonlite::toJSON(plot_information, pretty = T,auto_unbox = T)
  readr::write_file(plot_information, paste0(output_directory, 'plot_information.json'))
  
  # jsonlite::write_json(plot_information, pastIe0(output_directory, 'plot_information.json'))
  
  filters <- jsonlite::toJSON(filters, pretty = T,auto_unbox = T)
  readr::write_file(filters, paste0(output_directory, 'filters.json'))
  
  
  
  sf::st_write(spatial_data, paste0(output_directory, 'spatial_data.geojson'), append = F)
  
  
  
  
}


shape_file_directory <- './Local_Authority_Districts_May_2024_Boundaries_UK_BUC_-7430853278415417895/'
figure_directory <- './2_figures_new/'
seg_indice_directory <- './3_segregation_indices/'
output_directory <- 'outputs/'

prepare_shapefile(shape_file_directory,
                  figure_directory,
                  output_directory)
