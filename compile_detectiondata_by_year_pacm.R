# Script to compile formatted detection data for submission to PACM based on deployment year

### PROCESS:

# 1) Check that formatted detectiondata.csv files exist for each relevant deployment and species

# 2) Specify deployment year

# 3) Specify output folder for formatted csv (make sure this folder exists)

# 4) OPTIONAL: specify deployment folders to omit

# 4) Run script

### Edit these lines ----

year <- 2022

output_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\SUBMISSIONS\20260925_2022_metadata_baleen_beaked_INITIAL)"

# option to omit one or more deployments from compiled detectiondata (e.g., those using different time zone)
#omit_these <- c('SBVC1-2021-09', 'SBVC2-2021-09')

### Compile detectiondata csv ----

library(pacman)
p_load(tidyverse)

# Path to year folder
year_folder <- paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)", year)

# List all subfolders
deployment_folders <- list.dirs(year_folder, recursive = FALSE, full.names = TRUE)

# If "ignore" object exists, remove these deployment folders from list
if (exists('omit_these')){
  
  deployment_folders <- deployment_folders[!str_detect(deployment_folders, paste(omit_these, collapse = "|"))]

}

# List all detection data csv files
detection_files <- list.files(deployment_folders, pattern = "detectiondata", recursive = TRUE, full.names = TRUE)

# Initialize an empty list to store dataframes
dfs <- list()

# Read in each csv file and combine into single data frame
for (file in detection_files) {
  
  df <- read_csv(file)
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
}

compiled <- do.call(rbind, dfs)

### Output detection data csv in specified folder
write_csv(compiled, paste0(output_folder,"//detectiondata.csv"), na = "")
