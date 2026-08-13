# Basic script to create the citation RDS file for a publication, which will be used to match the paper to relevant 
# datasets and species in PACM submissions

# specify citation short code - should match code in citation_lookup.csv

citation_code = 'Feyrer_2024_a'

# if 'all_data' represents the full set of results used in the paper, and contains 'deployment' and 'species' variables:

dataset_summary <- beaked_data %>% 
  group_by(deployment, species) %>% 
  summarize() %>% 
  mutate(citation_code = citation_code)

# save RDS file in 'citation_rds' folder on NOAA_PACM GitHub repo

saveRDS(dataset_summary, here('citation_rds', paste0(citation_code, '.RDS')))
