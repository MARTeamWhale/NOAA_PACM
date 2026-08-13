# Example code to create the citation RDS file for a publication, which will be used to match the paper to relevant 
# datasets and species in PACM submissions


# specify citation short code - should match code in citation_lookup.csv

citation_code = 'Stanistreet_2022_a'


# EXAMPLE 1: if 'all_data' represents the full set of results used in the paper, and contains 'deployment' and 'species' variables

# get all unique combinations of deployment and species and add citation code
dataset_summary <- beaked_data %>% 
  group_by(deployment, species) %>% 
  summarize() %>% 
  mutate(citation_code = citation_code)


# EXAMPLE 2: if building from scratch for a small number of deployments:

# specify deployments and species manually:
deployment = c('EGL-2016-09', 'CGL-2016-09')
species = c('Ha', 'Mb', 'MmMe', 'Zc')

# expand all combinations and add citation code
dataset_summary <- expand_grid(deployment, species) %>% 
  mutate(citation_code = citation_code)


# save RDS file in 'citation_rds' folder on NOAA_PACM GitHub repo

saveRDS(dataset_summary, here('citation_rds', paste0(citation_code, '.RDS')))
