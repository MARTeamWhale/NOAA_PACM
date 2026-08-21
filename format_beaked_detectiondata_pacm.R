# Script to format beaked whale results for submission to PACM

### REQUIRED INPUT:

# Presence table in standard .xlsx format containing all species, named with "_Beaked_Presence.xlsx", and saved in deployment folder on OPP4
#   - if multiple single_species presence tables exist, these should be combined prior to running this script

### PROCESS:

# 1) Specify project, deployment name, year, list of species analyzed, missing data (true or false)

# 2) Check that formatted metadata csv and deployment folder for output exist on OPP3

# 3) Run script


#############################

# Info to edit:

project = 'DFO_MAR'
deployment = 'MGL_2017_12' # use underscores here to match folder names on OPP4
depl_year = 2017

# species included in analysis (options are Ha, Mb, MmMe, Zc)
species_list <- c('Ha', 'Mb', 'MmMe','Zc')

# specify TRUE if there are missing dates within deployment period (not accounted for in metadata)
missing_dates = FALSE

#############################

library(tidyverse)
library(here)
library(readxl)

##### 1) METADATA #####

#load formatted metadata file
metadata_file <- file.path('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/FORMATTED/', depl_year, '/', paste0('metadata_', depl_year, '.csv'))
metadata<- read_csv(metadata_file)

# get metadata for selected HF dataset
metadata_hf <- metadata %>% 
  mutate(deployment_name = str_extract(deployment_code, ".*(?=-[^-]*$)")) %>% 
  slice_max(order_by = recording_sample_rate_khz, by = deployment_name) %>% 
  filter(deployment_name == str_replace_all(deployment,"_", "-"))

# missing dates

if (missing_dates == TRUE){
  m_dates <- read_csv('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata/missing_dates.csv')
  
  missing <- m_dates %>%
    filter(deployment == metadata_hf$deployment_name) %>% 
    
    mutate(start_missing= as_date(as.character(start_missing)),
           end_missing= as_date(as.character(end_missing))) %>%

    rowwise() %>% 
    mutate(start_date = list(seq.Date(start_missing, end_missing, by = "day"))) %>% 
    unnest(cols=c(start_date)) %>%
    ungroup() %>% 
    select(-deployment, -start_missing, -end_missing) %>% 
    mutate(not_available = TRUE)
}


##### 2) BEAKED WHALE RESULTS #####

### load beaked whale results
input_file <- file.path('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_4/PAM_analysis/', project, '/', deployment, '/', paste0(deployment, '_Beaked_Presence.xlsx'))
dataset<- read_excel(input_file)
names(dataset) = sub(".*_","",names(dataset))

# tidy up data and fill in missing dates
tidy_dataset <- dataset %>% 
  
  # parse dates
  mutate(start_date = as_date(StartTime, format="%Y%m%d_%H%M%S")) %>% 
  
  # remove partial days
  filter(start_date>=metadata_hf$monitoring_start_datetime) %>% 
  filter(start_date<metadata_hf$monitoring_end_datetime) %>% 
  
  # change 'Me' to 'MmMe' if present
  rename(any_of(c(MmMe = 'Me'))) %>% 
  
  # format as tidy data
  pivot_longer(cols = any_of(species_list), names_to = "species", values_to = "presence") %>% 
  
  # fill in any missing species
  mutate(species = fct_expand(species, species_list)) %>% 
  
  # fill in presence
  group_by(start_date, species, .drop = F) %>% 
  summarise(true_count = sum(presence == "1"), possible_count = sum(presence == '-1')) %>% 
  ungroup() %>% 
  
  # fill in all dates
  complete(start_date = seq.Date(as_date(metadata_hf$monitoring_start_datetime),
                                 as_date(metadata_hf$monitoring_end_datetime-1), by="day"), 
           nesting(species), 
           fill = list(true_count = 0, possible_count = 0)) %>% 
  
  # add column specifying missing days
  {if (missing_dates) {
    left_join(., missing, by = "start_date") %>% 
      replace_na(list(not_available = FALSE)) %>% 
      
      # change presence to NA on missing days
      mutate(true_count = if_else(not_available %in% TRUE, NA_real_, true_count),
             possible_count = if_else(not_available %in% TRUE, NA_real_, true_count))
  } else {
    .
  }}

## named species list
pacm_species <- c("NBWH" = "Ha", "SOBW" = "Mb", "MMME" = "MmMe", "GOBW" = "Zc")


##### 3) CITATIONS #####

# load citation lookup csv from OPP3
citation_lookup <-read_csv('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/citation_lookup.csv')

# build citation table from rds files saved in PACM repo
citation_table <- list.files(path = here('citation_rds'), 
                         pattern = "\\.rds$", 
                         full.names = TRUE, 
                         ignore.case = TRUE) %>% 
  map_dfr(readRDS) %>% 
  filter(deployment == metadata_hf$deployment_name) %>% 
  left_join(citation_lookup) %>% 
  group_by(species) %>% 
  summarize(all_citations = paste(citation, collapse = '; '))


##### 4) FORMAT FOR PACM AND OUTPUT BY SPECIES #####

pacm_detections <- tidy_dataset %>% 
  
  left_join(citation_table, by = 'species') %>% 
  
  mutate(analysis_organization_code = 'DFO') %>% 
  
  mutate(deployment_code = metadata_hf$deployment_code) %>% 

  mutate(analysis_sound_source_codes = fct_recode(species, !!!pacm_species)) %>% 
  
  mutate(analysis_start_datetime = format_ISO8601(metadata_hf$monitoring_start_datetime, usetz = TRUE)) %>% 
  
  mutate(analysis_end_datetime = format_ISO8601(metadata_hf$monitoring_end_datetime, usetz = TRUE)) %>% 
  
  mutate(analysis_sample_rate_khz = metadata_hf$recording_sample_rate_khz) %>% 
  
  mutate(analysis_min_frequency_khz = 0) %>% 
  
  mutate(analysis_max_frequency_khz = analysis_sample_rate_khz/2) %>% 
  
  mutate(analysis_processing_code = 'POST_PROCESSED') %>% 
  
  mutate(analysis_protocol_reference = 'DFO Maritimes Beaked Whale Analysis Protocol') %>% 
  
  mutate(analysis_citations = all_citations) %>% 
  
  mutate(analysis_detector_code = 'TRITON_DFO_TWD') %>% 
  
  mutate(analysis_detector_version = 'Triton v1.0 2021 09 21; DFO TWD v1.3') %>% 
  
  mutate(detection_start_datetime = format_ISO8601(as_datetime(start_date), usetz = TRUE)) %>% 
  
  mutate(detection_end_datetime = format_ISO8601(as_datetime(start_date + 1), usetz = TRUE)) %>% 
  
  mutate(detection_effort_secs = metadata_hf$recording_duration_secs*(86400/metadata_hf$recording_interval_secs)) %>% 
  
  mutate(detection_sound_source_code = as_factor(analysis_sound_source_codes)) %>% 
  
  mutate(detection_call_type_code = 'OD_CLICK_FM') %>% 
  
  mutate(detection_n_validated = case_when(true_count >= 1 | possible_count >= 1 ~ 1,
                                           .default = NA)) %>% 
  
  mutate(detection_result_code = case_when(true_count >= 1 ~ 'DETECTED',
                                           true_count == 0 & possible_count >= 1 ~ 'POSSIBLY_DETECTED',
                                           true_count == 0 & possible_count == 0 ~ 'NOT_DETECTED',
                                           is.na(true_count) ~ 'NOT_AVAILABLE')) %>% 
  
  mutate(localization_method_code = '') %>% 
  mutate(localization_latitude = '') %>% 
  mutate(localization_longitude = '') %>% 
  mutate(localization_distance_m = '') %>% 
  
  
  transmute(analysis_organization_code,
            deployment_code,
            analysis_sound_source_codes,
            analysis_start_datetime,
            analysis_end_datetime,
            analysis_sample_rate_khz,
            analysis_min_frequency_khz,
            analysis_max_frequency_khz,
            analysis_processing_code,
            analysis_protocol_reference,
            analysis_citations,
            analysis_detector_code,
            analysis_detector_version,
            detection_start_datetime,
            detection_end_datetime,
            detection_effort_secs,
            detection_sound_source_code,
            detection_call_type_code,
            detection_n_validated,
            detection_result_code,
            localization_method_code,
            localization_latitude,
            localization_longitude,
            localization_distance_m)

### output by species
for (i in levels(pacm_detections$detection_sound_source_code)){
  
  # filter by species for export
  output <- pacm_detections %>% 
    filter(detection_sound_source_code == i) %>% 
    droplevels()
    
  # export csv files
  output_file <- file.path('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/FORMATTED/', depl_year, '/', metadata_hf$deployment_name, '/', 
                           paste0('detectiondata_', i, '.csv'))
    write_csv(output, file = output_file, na = "")
}


