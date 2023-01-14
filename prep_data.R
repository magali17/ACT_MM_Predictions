
pacman::p_load(tidyverse)


###########################################################################################
# UPLOAD MODELING DATA
###########################################################################################
annual <- readRDS(file.path("data", "original", "annual.rda")) %>%
  #only want this averaging approach
  filter(annual == "mean_of_win_medians") %>%
  select(-annual) %>%
  # will be modeling on the log scale (cannot have 0s)
  mutate(value = ifelse(value==0, 0.001, value),
         value = log(value))

vars <- unique(annual$variable)

# save datasets for each pollutant
lapply(vars, function(x) {
  filter(annual, variable==x) %>%
    saveRDS(., file.path("data", paste0("annual_", x, ".rda")))
  })
