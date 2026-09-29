# Script to compile formatted detection data for submission to PACM for specified deployments and (optionally) species

#**** IMPORTANT - THIS SCRIPT IS ONLY FOR COMPILING DETECTION DATA ANALYZED IN LOCAL TIME ****

### PROCESS:

# 1) Check that formatted detectiondata.csv files exist for each relevant deployment and species

# 2) Specify deployment(s)

# 3) OPTIONAL: specify species to include (default is all species with detection data available)

# 4) specify timezone

# 5) Specify output folder for formatted csv (make sure this folder exists)

# 6) Run script

### Edit these lines ----

# specify deployment(s)
deployments <- c('SBVC1-2021-09', 'SBVC2-2021-09')

# select particular species to include? if TRUE, specify list below. if FALSE, all species with detection data will be included.
select_species <- FALSE

# if select_species = TRUE, specify list of species to include
species <- c('BLWH', 'FIWH')

# specify timezone
local_timezone <- "America/Halifax"

output_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\SUBMISSIONS\TEST)"

### Compile detectiondata csv ----

library(pacman)
p_load(tidyverse)
p_load(here)

# Path to parent folder with all formatted data
main_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)"

# List all subfolders
deployment_folders <- list.dirs(main_folder, recursive = TRUE, full.names = TRUE)

# Find selected deployment folders
select_depl<- deployment_folders[str_detect(deployment_folders, paste(deployments, collapse = "|"))]

# If select_species = TRUE
if (select_species){
  
  detection_files <- vector()
  
  # find detection data files for selected species in selected deployments
  for (sp in species) {
    output <- list.files(select_depl, pattern = paste0("detectiondata_", sp), recursive = TRUE, full.names = TRUE)
    detection_files<- c(detection_files, output)
    
  }
  
}  else {
  
  # find detection files for all species in selected deployments
  detection_files <- list.files(select_depl, pattern = "detectiondata", recursive = TRUE, full.names = TRUE)
  
}

# Initialize an empty list to store dataframes
dfs <- list()

# Read in each csv file and combine into single data frame
for (file in detection_files) {
  
  df <- read_csv(file, locale = locale(tz = local_timezone)) %>% 
    
    # re-format datetime fields
    mutate(analysis_start_datetime = format_ISO8601(as_datetime(analysis_start_datetime),usetz = TRUE),
           analysis_end_datetime = format_ISO8601(as_datetime(analysis_end_datetime),usetz = TRUE),
           detection_start_datetime = format_ISO8601(as_datetime(detection_start_datetime),usetz = TRUE),
           detection_end_datetime = format_ISO8601(as_datetime(detection_end_datetime),usetz = TRUE))
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
}

compiled <- do.call(rbind, dfs)

### Output detection data csv in specified folder -----
write_csv(compiled, paste0(output_folder,"//detectiondata.csv"), na = "")
