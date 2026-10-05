# Script to compile formatted detection data for submission to PACM based on citation RDS

#**** IMPORTANT - THIS SCRIPT IS ONLY FOR COMPILING DETECTION DATA IN UTC ****

### PROCESS:

# 1) Check that formatted detectiondata.csv files exist for each relevant deployment and species, and are already updated
#    with the new citation.

# 2) Specify citation RDS file

# 3) Specify output folder for formatted csv (make sure this folder exists)

# 4) Run script

### Edit these lines ----

# specify citation RDS file containing list of deployments & species
citation_rds_file <- 'Stanistreet_2026_a.RDS'

# specify output folder
output_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\SUBMISSIONS\TEST)"

### Read in citation RDS -----

library(pacman)
p_load(tidyverse)
p_load(here)

# read in RDS
deployment_list <- readRDS(here('citation_RDS', citation_rds_file))
#deployment_list <- test

# named species list
pacm_species <- c("NBWH" = "Ha", "SOBW" = "Mb", "MMME" = "MmMe", "GOBW" = "Zc",
                  "BLWH" = "Bm", "FIWH" = "Bp", "SEWH" = "Bb", "HUWH" = "Mn", "RIWH" = "Eg", "MIWH" = "Ba")

# convert species to PACM codes
deployment_list <- deployment_list %>% 
  mutate(pacm_species = fct_recode(species, !!!pacm_species))

### Check for datasets in local time -----

# read deployments in local time
depl_local<-read_csv(here('deployments_localtime.csv'))

# check if any specified deployments are in this list
omit_these <- intersect(deployment_list$deployment, depl_local$deployment)

### Compile detectiondata csv ----

# Path to parent folder with all formatted data
main_folder <- r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)"

# List all subfolders
deployment_folders <- list.dirs(main_folder, recursive = TRUE, full.names = TRUE)

# Find selected deployment folders
select_depl<- deployment_folders[str_detect(deployment_folders, paste(deployment_list$deployment, collapse = "|"))]

# If "omit_these" object exists, remove these deployment folders from list
if (!is_empty(omit_these)){
  
  select_depl <- select_depl[!str_detect(select_depl, paste(omit_these, collapse = "|"))]
  
}

# Create list of detection files to include

detection_files <- vector()
  
for (df in select_depl){
  
  # deployment name
  depl_name <- basename(df)
  
  # get list of species to include from this deployment
  depl_sp <- deployment_list %>% 
    filter(deployment == depl_name) %>% 
    pull(pacm_species) %>% 
    as.character()
  
  # find detection data files for selected species in selected deployment
  for (sp in depl_sp) {
    output <- list.files(df, pattern = paste0("detectiondata_", sp), recursive = TRUE, full.names = TRUE)
    detection_files<- c(detection_files, output)
    
  }
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
if (!is_empty(omit_these)){
  
  warning("Deployments in local time were omitted from results: ", omit_these, call.=FALSE)
  
}
