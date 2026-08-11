
library(pacman)

p_load(tidyverse)

### PROCESS:

# 1) Specify deployment code, missing data (true or false)

# 2) Check that formatted metadata csv and deployment folder for output exist on OPP3

# 3) Run script

# Edit These ----

#Use underscores
deployment = ''

missing.dates = FALSE

#############################################################

# Metadata ----

year <- str_sub(deployment, start = -7, end = -4)

#bring in deployment
metadata.in <- read_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_3\NOAA_PACM_Data\FORMATTED\)",year,"\\metadata_",year,".csv"))

metadata <- metadata.in %>%
  filter(str_detect(deployment_code,str_replace_all(deployment, "_","-"))) %>% # filter metadata for matching deployment
  slice_min(recording_sample_rate_khz)

#############################################################

# Data input ----
data.in <- read_csv(paste0(r"(R:\Science\CetaceanOPPNoise\CetaceanOPPNoise_5\BaleenWhale_AcousticAnalysis\Deployments\MAR\)",deployment,"\\Results\\",deployment,"_baleenwhale_dailypresence.csv"))

sp.codes <- c("Bm"="BLWH","Bp"="FIWH","Bb"="SEWH","Mn"="HUWH","Eg"="RIWH","Ba"="MIWH")

call.codes <- c("BLWH"="BLWH_MIX","FIWH"="FIWH_MIX","SEWH"="SEWH_DS80HZ","HUWH"="HUWH_MIX","RIWH"="RW_MIX","MIWH"="MIWH_PT")

presence.codes <- c("D"="DETECTED","P"="POSSIBLY_DETECTED","N"='NOT_DETECTED')


data <- data.in %>% 
  select(-calltype) %>% 
  
  group_by(detecdate,species) %>% 
  arrange(presence) %>% 
  slice(1) %>% 
  ungroup() %>% 
  
  mutate(species = recode(species, !!!sp.codes)) %>% 
  
  complete(detecdate = seq.Date(as.Date(metadata$monitoring_start_datetime),as.Date(metadata$monitoring_end_datetime)-1, by="day"), nesting(species), fill=list(presence="N")) %>% 
  
  mutate(callcat = call.codes[species])
  
  
  
###############################################################

# Table building ----

max.freq.codes <- c('8'='4', '32'='16', '256'='4')


for (i in sp.codes){
  
PACM_detections <- data %>% 
  filter(species ==i) %>% 
  mutate(analysis_organization_code = "DFO",
  deployment_code = metadata$deployment_code,
  analysis_sound_source_codes= i,
  analysis_start_datetime	= format_ISO8601(as_datetime(metadata$monitoring_start_datetime)),
  analysis_end_datetime	= format_ISO8601(as_datetime(metadata$monitoring_end_datetime)),
  analysis_sample_rate_khz = 	metadata$recording_sample_rate_khz,
  analysis_min_frequency_khz	= 0,
  analysis_max_frequency_khz = 	as.numeric(max.freq.codes[as.character(metadata$recording_sample_rate_khz)]),
  analysis_processing_code = "POST_PROCESSED",	
  analysis_protocol_reference	= "DFO Team Whale Baleen Whale Analysis Protocols") %>% 
  
  mutate(analysis_citations= "")	%>% 
    
  mutate(analysis_detector_code = case_when(i=="MIWH"~"MANUAL", TRUE~"LFDCS"),
         analysis_detector_version	= case_when(i=="MIWH"~"", TRUE~"gom9_TW")) %>% 
  
  mutate(detection_start_datetime = format_ISO8601(as_datetime(detecdate)),
         detection_end_datetime = format_ISO8601(as_datetime(detecdate+1)),
         detection_effort_secs =  metadata$recording_duration_secs*(86400/metadata$recording_interval_secs)) %>% 
  
  mutate(detection_sound_source_code = species,
         detection_call_type_code = callcat,
         detection_n_validated = case_when(presence == 'D'~1, 
                                           presence=='P'~1,
                                           presence=='N'~0, .default = NA),
         detection_result_code = presence) %>% 
  
  select(-c(detecdate,species,callcat,presence)) %>% 
  
  mutate(localization_method_code="",	
         localization_latitude=""	,
         localization_longitude="",	
         localization_distance_m="") %>% 
  
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

# export csv files
output_file <- file.path('R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_3/NOAA_PACM_Data/FORMATTED/', year, '/', str_replace_all(deployment, "_","-"), '/', 
                         paste0('detectiondata_', i, '.csv'))
write_csv(PACM_detections, file = output_file, na = "")
  	}
  
