
# Script to format deployment metadata for submission to PACM: specific deployments recorded in local time rather than UTC

### PROCESS:

# 1) Make sure deployment_summary.csv report saved on OPP2 is up to date (re-export full report from metadatabase app if needed).

# 2) Specify deployment year, specific deployment(s) to format, and timezone to use

# 3) Specify output folder for formatted csv (make sure this folder exists)

# 4) Run script

# 5) Open csv, check results for any missing info, and fill in these columns:

#   - recording_device_depth_m (look at mooring diagrams)
#   - recording_bit_depth (equipment > channel properties in metadatabase app)
#   - check dataset notes and made any edits needed (e.g., usable data end date)
#   - *** FOR SOUNDTRAPS - if multiple recorders were used to create one dataset, edit recording_device_code and recording schedule info appropriately

#############################

# Info to edit:

DeploymentYear <- 2021

TimeZone <- "America/Halifax"

Deployments <-c('SBVC1-2021-09', 'SBVC2-2021-09')

OutputFolderPath <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/FORMATTED/2021'


#############################

library(tidyverse)
library(here)

depl_summary <- read_csv(r'(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\PAM_metadata\deployment_summary.csv)') 
rec_schedules <- read_csv(r'(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_2\PAM_metadata\recording_schedules.csv)')

pacm_metadata <- depl_summary %>% 
  filter(Year == DeploymentYear) %>% 
  filter(Deployment %in% Deployments) %>% 
  
  mutate(deployment_organization_code = 'DFO') %>% 
  
  mutate(deployment_code = Deployment) %>% 
  
  mutate(project_name = 'DFO Maritimes Cetacean Monitoring') %>% 
  
  mutate(site_code = str_extract(Station, '[^:]+')) %>%
  
  mutate(monitoring_start_datetime = format_ISO8601(ceiling_date(as.POSIXct(`In-water_start`, tz = TimeZone), unit = 'day'), usetz = TRUE)) %>% 
  
  mutate(monitoring_end_datetime = format_ISO8601(floor_date(as_datetime(`In-water_end`, tz = TimeZone), unit = 'day'), usetz = TRUE)) %>% 
  
  mutate(deployment_latitude = Latitude) %>% 
  
  mutate(deployment_longitude = Longitude) %>% 
  
  mutate(deployment_platform_type_code = 'BOTTOM_MOUNTED_MOORING') %>% 
  
  mutate(deployment_platform_id = '') %>% 
  
  mutate(deployment_water_depth_m = Depth_m) %>% 
  
  mutate(recording_device_depth_m = '') %>% 
  
  mutate(recording_device_code = str_extract(`Equipment make_model_serial`, "(?<=- )[^-]+(?=- )")) %>% 
  
  mutate(recording_device_type_code = case_when(str_detect(`Equipment make_model_serial`, "AMAR") ~ 'AMAR',
                                                str_detect(`Equipment make_model_serial`, "SoundTrap") ~ 'SOUNDTRAP',
                                                .default = 'CHECK')) %>% 
  
  ### match recording schedules with lookup table
  mutate(recording_schedule = str_extract(`Recording schedule`, ".*(?=\\s:)")) %>% 
  
  left_join(rec_schedules, by= join_by(recording_schedule)) %>% 
  
  # replicate rows per deployment based on number of sampling rates
  uncount(n_sampling_rates) %>% 
  
  # identify row for each sampling rate
  group_by(deployment_code) %>% 
  mutate(rec_stage = row_number()) %>% 
  ungroup() %>% 
  
  mutate(recording_duration_secs = case_when(rec_stage == 1 ~ recording_duration_1,
                                             rec_stage == 2 ~ recording_duration_2,
                                             rec_stage == 3 ~ recording_duration_3,
                                             .default = NA)) %>% 
  
  mutate(recording_interval_secs = period_seconds) %>% 
  
  mutate(recording_sample_rate_khz = case_when(rec_stage == 1 ~ sampling_rate_1,
                                               rec_stage == 2 ~ sampling_rate_2,
                                               rec_stage == 3 ~ sampling_rate_3,
                                               .default = NA)) %>% 
  
  
  mutate(deployment_code = str_c(deployment_code, '-', recording_sample_rate_khz)) %>% 
  
  mutate(recording_bit_depth = '') %>% 
  
  mutate(recording_n_channels = n_channels) %>% 

  mutate(recording_timezone = TimeZone) %>% 
  
  mutate(dynamic_management_platform = FALSE) %>% 
  
  mutate(deployment_url = '') %>% 
  
  mutate(points_of_contact = 'Hilary Moors-Murphy <Hilary.Moors-Murphy@dfo-mpo.gc.ca') %>% 
  
  mutate(project_funding = '') %>% 
  
  # clean up and organize
  
  transmute(deployment_organization_code,
            deployment_code,
            project_name,
            site_code,
            monitoring_start_datetime,
            monitoring_end_datetime,
            deployment_latitude,
            deployment_longitude,
            deployment_platform_type_code,
            deployment_platform_id,
            deployment_water_depth_m,
            recording_device_depth_m,
            recording_device_code,
            recording_device_type_code,
            recording_duration_secs,
            recording_interval_secs,
            recording_sample_rate_khz,
            recording_bit_depth,
            recording_n_channels,
            recording_timezone,
            dynamic_management_platform,
            deployment_url,
            points_of_contact,
            project_funding)
  

# export csv
output_file <- file.path(OutputFolderPath, paste0("metadata_", DeploymentYear, "_localtime.csv"))
write_csv(pacm_metadata, file = output_file)
  
