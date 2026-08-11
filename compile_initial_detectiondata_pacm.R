library(pacman)

p_load(tidyverse)

# Edit these ----

year = 

output.folder = r"()"

# Bring in all detection csvs ----

# Set the path to your main folder
main_folder <- paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)",year) # direct to all deployments

# List all subfolders
detection.files <- list.files(main_folder, pattern = "detectiondata", recursive = TRUE, full.names = TRUE) #list all subfolders (aka Validation, Results)

# Initialize an empty list to store dataframes
dfs <- list()

# Iterate over each subfolder, read the CSV files, and combine into a single dataframe
for (file in detection.files) {
  
  df <- read_csv(file) #read presence csv
  
  # Store the dataframe in the list
  dfs[[length(dfs) + 1]] <- df
  
}

compiled <- do.call(rbind, dfs)

write_csv(compiled, paste0(output.folder,"//detectiondata.csv"))
