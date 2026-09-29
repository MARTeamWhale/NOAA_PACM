# Script to compile formatted detection data for submission to PACM for specified deployments and (optionally) species

#**** IMPORTANT - THIS SCRIPT IS ONLY FOR COMPILING DETECTION DATA IN UTC ****

### PROCESS:

# 1) Check that formatted detectiondata.csv files exist for each relevant deployment and species

# 2) Specify deployment(s)

# 3) OPTIONAL: specify species to include (default is all species with detection data available)

# 4) Specify output folder for formatted csv (make sure this folder exists)

# 5) Run script

### Edit these lines ----

# specify deployment(s)
deployments <- c('MBK-2023-08')

# select particular species to include? if TRUE, specify list below. if FALSE, all species with detection data will be included.
select_species <- TRUE

# if select_species = TRUE, specify list of species to include
species <- c('BLWH')

# specify output folder
output_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\SUBMISSIONS\TEST)"

### Check for datasets in local time -----

library(pacman)
p_load(tidyverse)
p_load(here)

# read deployments in local time
depl_local<-read_csv(here('deployments_localtime.csv'))

# check if any specified deployments are in this list
omit_these <- intersect(deployments, depl_local$deployment)

### Compile detectiondata csv ----

# Path to parent folder with all formatted data
main_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)"

# List all subfolders
deployment_folders <- list.dirs(main_folder, recursive = TRUE, full.names = TRUE)

# Find selected deployment folders
select_depl<- deployment_folders[str_detect(deployment_folders, paste(deployments, collapse = "|"))]

# If "omit_these" object exists, remove these deployment folders from list
if (length(omit_these>1)){
  
  select_depl <- select_depl[!str_detect(select_depl, paste(omit_these, collapse = "|"))]
  
}

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
  
  df <- read_csv(file)
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
}

compiled <- do.call(rbind, dfs)

### Output detection data csv in specified folder -----
write_csv(compiled, paste0(output_folder,"//detectiondata.csv"), na = "")

# if deployments in local time were omitted, print warning
if (length(omit_these>1)){
  
  warning("Deployments in local time were omitted from results: ", omit_these, call.=FALSE)
  
}
