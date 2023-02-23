# script drops some pollutants and log transforms values 


##################################################################################################
# SETUP
##################################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse)
###########################################################################################
# DIRECTORIES
###########################################################################################
if(!dir.exists(file.path("data", "output", "specific_pollutants"))) {dir.create(file.path("data", "output", "specific_pollutants"), recursive = T)}
if(!dir.exists("output")) {dir.create("output")}


###########################################################################################
# UPLOAD MODELING DATA
###########################################################################################
covariates <- readRDS(file.path("data", "output", "modeling_geocovariates.rda"))

annual <- readRDS(file.path("data", "output", "annual_avgs.rda")) %>%
  
  # 2 largest bins have missingness > 95% of the time. 3rd largest bin has missingness 53% of the time
  filter(!variable %in% paste0("ns_", c("205.4", "273.8", "365.2"))) %>%
  
  # will be modeling on the log scale (cannot have 0s)
  # mutate(
  #   value = ifelse(value==0, 0.001, value),
  #   value = log(value)
  #        ) %>%
  
  # add modeling covariat3s
  left_join(covariates, by = "location") %>%
  ungroup()

saveRDS(annual, file.path("data", "output", "annual_avgs_and_cov.rda"))

# save datasets for each pollutant
vars <- unique(annual$variable)

lapply(vars, function(x) {
  filter(annual, variable==x) %>%
    saveRDS(., file.path("data", "output", "specific_pollutants", paste0("annual_avg_", x, ".rda")))
  })

# save modeling covariates
select(annual, log_m_to_a1:ll_a23_s05000) %>%
  names() %>%
  saveRDS(file.path("data", "output", "modeling_covariates.rda"))

