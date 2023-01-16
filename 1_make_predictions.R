# Rscript 1_make_predictions.R data/annual_ns_total_conc.rda ../../../dr1008/sea_grid_100m output/nancy/ns_total_conc_sea_grid_100m csv

################################################################################
# ABOUT THIS SCRIPT
################################################################################
# AUTHOR, DATE: Magali Blanco, 1/13/2023

# OBJECTIVE
# Program takes a training data set and a set of new locations with geographic covaraites, and it returns predictions for those locations

# INPUTS
# 4 inputs are required: 
#   1. the file path for the modeling data 
#   2. the file path for the geocovariate dataset where predictions are desired
#   3. the repository where prediction files should be saved
#   4. the desired prediction file format, either csv or rda 

# OUTPUTS
# The output is a dataset with annual average air pollution predictions for the locations with geographic covariates. 

# ERROR/WARNING MESSAGES
# Error messages occur if:
#   1. four arguments are not included when running this program
#   2. there are locations with missing covariate values or missing covariates altogether that are required for the prediction models to run

# EXAMPLE OF HOW TO USE THIS PROGRAM
# in a terminal open to the R program project directory, type: 
# Rscript 1_make_predictions.R  <modeling_data_path> <covariate_file_path> <prediction_directory> <prediction_file_format>
## Rscript 1_make_predictions.R data/annual_ns_total_conc.rda ../../HEI/Aim 3a - health inferences from diff exposure surfaces/R - hei_aim3a/data/dr0311_grid_covars.rda output/dr0311_grid rda


################################################################################
# SETUP
################################################################################
# Clear workspace of all objects and unload all extra (non-base) packages
rm(list = ls(all = TRUE))
if (!is.null(sessionInfo()$otherPkgs)) {
  res <- suppressWarnings(
    lapply(paste('package:', names(sessionInfo()$otherPkgs), sep=""),
           detach, character.only=TRUE, unload=TRUE, force=TRUE))
}

# load the required libraries for: plotting, modeling, spatial features, script timing
if (!require("pacman")) {install.packages("pacman")}
pacman::p_load(tidyverse, ggpubr, pls, gstat, sf, ggspatial, tictoc, tools,parallel)

set.seed(1)

source("functions.R")

# report how long script takes to run
tic()
###########################################################################################
# TAKE IN USER ARGUMENTS 
###########################################################################################
#allow R to take input from the command line
user_arguments <- commandArgs(trailingOnly = TRUE)

# user_arguments <- c("data/annual_ns_total_conc.rda", "../../../dr1008/sea_grid_100m.csv", "output/nancy/ns_total_conc_sea_grid_100m", "csv")

if (length(user_arguments) !=4) {
  print("Usage error. Enter: 1. the location of the covariate dataset for which you would like predictions, 2. where the prediction outputs should be saved, and 3. the desired prediction file fomat (csv or rda). Usage:")
  print("Rscript 1_make_predictions.R  <modeling_data_path> <covariate_file_path> <prediction_directory> <prediction_file_format>")
  print("Note. Use ' ' to include spaces in file paths")
  stop()
}

# new covariate file
modeling_data_path <- user_arguments[1]
covariate_file_path <- user_arguments[2]
cov_ext <- tools::file_ext(covariate_file_path)

#where predictions should be saved
prediction_directory <- user_arguments[3]
## create the directory if it does not already exists
if(!dir.exists(prediction_directory)) {dir.create(prediction_directory, recursive = TRUE)}

# the prediction file format (e.g., 'rda')
prediction_file_format <- tolower(user_arguments[4])

###########################################################################################
# SAVE USER TERMINAL INPUTS
###########################################################################################
write.table(paste("Rstudio 1_make_predictions.R", paste(user_arguments, collapse = " ")), 
            file = file.path(prediction_directory, "user_arguments.txt"), row.names = F, col.names = F, quote = F)

###########################################################################################
# UPLOAD THE NEW DATASET WHERE PREDICTIONS ARE DESIRED
###########################################################################################
if(cov_ext == "rda") {dt0 <- readRDS(covariate_file_path)}
if(cov_ext == "csv") {dt0 <- read.csv(covariate_file_path)}

if(!cov_ext %in% c("csv", "rda")) {stop("Error. Covariate file must be a CSV or RDA file")}

###########################################################################################
# UPLOAD MODELING DATA
###########################################################################################
lat_long_crs <- 4326

# modeling data is on log scale
modeling_data <- readRDS(modeling_data_path) #%>% st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs, remove=F)

# # the covariate names that will be used in the model
cov_names <- readRDS(file.path("data", "modeling_covariates.rda"))

# load a spatial file of the original monitoring area to assess spatial extrapolation later
monitoring_area <- readRDS(file.path("data", "original", "monitoring_land_zero_water_shp.rda"))  

###########################################################################################
# Blanco et al. 2022 campaign paper uses 3 PLS scores
pls_comp_n <- 3

###########################################################################################
# GENERATE NEW COVARIATES FOR THE DATASET
###########################################################################################
dt <- generate_new_vars(dt0) %>%
  st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs, remove=F)

###########################################################################################
# ADD LOCATION INDICATORS
###########################################################################################
# add indicators of whether or not the prediction locations are in the monitoring area
dt$in_monitoring_area <- suppressMessages(
  st_intersects(dt, monitoring_area, sparse = F) %>%
    apply(., 1, any))

###########################################################################################
# QC CHECKS
###########################################################################################
# 1. ensure that the new dataset has all the necessary geograpahic covariates for modeling, otherwise give an error message with the missing covariates
has_all_covariates <- all(cov_names %in% names(dt))

if(has_all_covariates==FALSE) {
  missing_cov <- cov_names[!cov_names %in% names(dt)]
  error_msg <- paste("The following covariates are needed but missing from the dataset:", paste(missing_cov, collapse = ", "), ". Please fix this before continuing.")
  stop(error_msg)
}

# 2. check that there are no missing covariate values for any location
has_missing_values <- sapply(dt[cov_names], function(x) any(is.na(x) )) %>% 
  as.data.frame() %>% rownames_to_column() %>%
  rename(true_or_false = '.')  

if(any(has_missing_values$true_or_false) == TRUE) {
  covariates_with_missingness <- filter(has_missing_values, true_or_false==TRUE) %>%
    pull(rowname)
  
  error_msg <- paste("The following covariates have 1+ rows with missing values:", paste(covariates_with_missingness, collapse = ", "), ". These rows will have missing predictions.")
  
  print(error_msg)
  #stop(error_msg)
}

# 3. print a 'pass' message if all of the covariates are present and there are no locations with missing values
if(has_all_covariates ==TRUE & any(has_missing_values$.) == FALSE) {print("Covariate checks passed.")} 

###########################################################################################
# PREDICT AT NEW DATASET
###########################################################################################
print("Generating predictions...")

new_data = dt %>%
  mutate(variable = first(modeling_data$variable))

new_predictions0 <- mclapply(group_split(modeling_data, variable),
                             mc.cores = 4,
                             function(x) {
                               set.seed(1)
                               temp <- dt %>%
                                 mutate(variable = first(x$variable)) %>%
                                 uk_pls(new_data = ., modeling_data = x)}) %>%
  bind_rows()  

# save the location and prediction information
new_predictions <- new_predictions0 %>%
  # modeling data is in log scale. convert back to native scale
  mutate(prediction = exp(prediction)) %>%
  st_drop_geometry()

###########################################################################################
# SAVE THE PREDICTIONS
###########################################################################################
prediction_file_name <- file.path(prediction_directory, paste0("predictions.", prediction_file_format))

if(prediction_file_format == "csv") {write.csv(new_predictions, prediction_file_name, row.names = F)}
if(prediction_file_format == "rda") {saveRDS(new_predictions, prediction_file_name)}

print(paste0("Predictions saved: ", prediction_file_name))


###########################################################################################
#print the script run duration
toc()

print("PROGRAM DONE")

###########################################################################################
