# Script to format beaked whale results for submission to PACM

### PROCESS:

# 1) Specify project, deployment name, year, list of species analyzed, missing data (true or false)

# 2) Check that formatted metadata csv and deployment folder for output exist on OPP3

# 3) Run script

## JOY TO ADD: citations based on look up table(s), code to handle missing dates, cases with multiple presence tables


#############################

# Info to edit:

project = 'DFO_MAR'
deployment = 'EGL_2016_09' # use underscores here to match folder names on OPP4
depl_year = 2016

# species included in analysis - options are Ha, Mb, MmMe, Zc
species_list <- c('Ha', 'Mb', 'MmMe', 'Zc')

missing_dates = FALSE

#############################

library(tidyverse)
library(here)
library(readxl)

### load formatted metadata file
metadata_file <- file.path('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/FORMATTED/', depl_year, '/', paste0('metadata_', depl_year, '.csv'))
metadata<- read_csv(metadata_file)

# get metadata for selected HF dataset
metadata_hf <- metadata %>% 
  mutate(deployment_name = str_extract(deployment_code, ".*(?=-[^-]*$)")) %>% 
  slice_max(order_by = recording_sample_rate_khz, by = deployment_name) %>% 
  filter(deployment_name == str_replace_all(deployment,"_", "-"))

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
  
  # fill in missing dates
  complete(start_date = seq.Date(as_date(metadata_hf$monitoring_start_datetime),
                                 as_date(metadata_hf$monitoring_end_datetime-1), by="day"), 
           nesting(species), 
           fill = list(true_count = 0, possible_count = 0))


### format detection data for pacm
pacm_detections <- tidy_dataset %>% 
  
  mutate(analysis_organization_code = 'DFO') %>% 
  
  mutate(deployment_code = metadata_hf$deployment_code) %>% 
  
  mutate(analysis_sound_source_codes = case_when(species == 'Ha' ~ 'NBWH',
                                                 species == 'Mb' ~ 'SOBW',
                                                 species == 'MmMe' ~ 'MMME',
                                                 species == 'Zc' ~ 'GOBW')) %>% 
  
  mutate(analysis_start_datetime = format_ISO8601(metadata_hf$monitoring_start_datetime, usetz = TRUE)) %>% 
  
  mutate(analysis_end_datetime = format_ISO8601(metadata_hf$monitoring_end_datetime, usetz = TRUE)) %>% 
  
  mutate(analysis_sample_rate_khz = metadata_hf$recording_sample_rate_khz) %>% 
  
  mutate(analysis_min_frequency_khz = 0) %>% 
  
  mutate(analysis_max_frequency_khz = analysis_sample_rate_khz/2) %>% 
  
  mutate(analysis_processing_code = 'POST_PROCESSED') %>% 
  
  mutate(analysis_protocol_reference = 'DFO Maritimes Beaked Whale Analysis Protocol') %>% 
  
  mutate(analysis_citations = '') %>% 
  
  mutate(analysis_detector_code = 'TRITON_CLICK') %>% 
  
  mutate(analysis_detector_version = 'Triton version 1.0 2021 09 21') %>% 
  
  mutate(detection_start_datetime = format_ISO8601(as_datetime(start_date), usetz = TRUE)) %>% 
  
  mutate(detection_end_datetime = format_ISO8601(as_datetime(start_date + 1), usetz = TRUE)) %>% 
  
  mutate(detection_effort_secs = metadata_hf$recording_duration_secs*(86400/metadata_hf$recording_interval_secs)) %>% 
  
  mutate(detection_sound_source_code = as_factor(analysis_sound_source_codes)) %>% 
  
  mutate(detection_call_type_code = 'OD_CLICK_FM') %>% 
  
  mutate(detection_n_validated = case_when(true_count >= 1 | possible_count >= 1 ~ 1,
                                           .default = NA)) %>% 
  
  mutate(detection_result_code = case_when(true_count >= 1 ~ 'DETECTED',
                                           true_count == 0 & possible_count >= 1 ~ 'POSSIBLY_DETECTED',
                                           true_count == 0 & possible_count == 0 ~ 'NOT_DETECTED')) %>% 
  
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


