# these results should be similar to what is in Our Campaign R (main ACT-TRAP paper), 
# but this includes additional variables and 10FCV using all 309 sites

###########################################################################################
# SETUP
###########################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

pacman::p_load(tidyverse, pls, gstat, sf, parallel)

set.seed(1)

#functions
source("functions.R")
#source("functions_hei3a.R")

use_cores <- 4

if(!dir.exists(file.path("data", "output", "objects"))) {dir.create(file.path("data", "output", "objects"), recursive = T)}

###########################################################################################
# UPLOAD DATA
###########################################################################################
annual <- readRDS(file.path("data", "output", "annual_avgs_and_cov.rda")) %>%
  # folds for CV
  add_random_fold() %>%
  # will be modeling on the log scale (cannot have 0s)
  mutate(value = ifelse(value==0, 0.001, value),
         value = log(value)) 

# # # from Blanco 2022 ACT campaign paper
# pls_comp_n <- 3
# saveRDS(pls_comp_n, file = file.path("data", "output", "objects", "pls_comp_n.rda"))

# #k-folds for CV
# k <- 10

##################################################################################################
# CV PREDICTIONS
##################################################################################################
common_names <- c("location", "variable", "value", "prediction", "out_of_sample") 

# cross-validation
cv_predictions <- mclapply(group_split(annual, variable), 
                           FUN = do_cv, 
                           mc.cores = 1#use_cores
                           ) %>%
  bind_rows() %>%
  mutate(out_of_sample = "cross-validation") %>%
  select(all_of(common_names)) %>%
  # put values & predictions back on the native scale before evaluating
  mutate(value = exp(value),
         prediction = exp(prediction))

##################################################################################################
# MODEL EVALUATION
##################################################################################################

# --> regression and MSE-based R2 are always the same here?

model_evaluation <- mclapply(group_split(cv_predictions, variable), 
                              validation_stats, 
                              mc.cores = 1, #use_cores
                              ) %>%
  bind_rows() %>%
  mutate_if(is.numeric, ~round(., 2))

write.csv(model_evaluation, file.path("data", "output", "model_evaluation.csv"), row.names = F)



