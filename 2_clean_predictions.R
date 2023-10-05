
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

pacman::p_load(dplyr, readr, lubridate)    

prediction_path <- file.path("output", "cohort"
                             #"dr0311_grid",  
                             )

if(!dir.exists(file.path(prediction_path, "KP"))){dir.create(file.path(prediction_path, "KP"), recursive = T)}

##################################################################################################
#  LOAD DATA
##################################################################################################

new_variables <- c("pnc_20_36",
                   "pmdisc_size",
                   "ma200_blue_bc1", "ma200_green_bc1", "ma200_red_bc1", "ma200_uv_bc1"#, # "ma200_ir_bc1"
                   
                   #NS bins, added 10/4/23
                   #"ns_10_100", "ns_11.5", "ns_115.5", "ns_15.4", "ns_154.0", "ns_20.5", "ns_27.4", "ns_36.5", "ns_48.7", "ns_64.9", "ns_86.6"
                   )
  
# predictions0 <- readRDS(file.path(prediction_path, "predictions.rda")) %>%
#   filter(variable %in% new_variables)

message("loading individual model/pollutant predictions")
predictions0 <- lapply(new_variables,
                       function(x) {readRDS(file.path(prediction_path, x, "predictions.rda"))}) %>%
  bind_rows()

##################################################################################################
#  CLEAN PREDICTIONS
##################################################################################################

predictions <- predictions0 %>%
  
  # only predict at locations in monitoring area
  filter(in_monitoring_area,
         !is.na(prediction)) %>%
  
  mutate(
    # The start and end date is the valid period during which the model can be applied to homes. These dates match PM2.5 and NO2
    start_date = ymd("1988-01-01 "),
    end_date = ymd("2021-07-09 "),
    model = variable,
  ) %>%
  select(location_id, start_date, end_date, model, prediction)

message("saving predictions under:")
print(file.path(prediction_path, "KP", paste0("predictions_additional_vars_", Sys.Date())))
      
saveRDS(predictions, file.path(prediction_path, "KP", paste0("predictions_additional_vars_", Sys.Date(),".rda")))
write_csv(predictions, file.path(prediction_path, "KP", paste0("predictions_additional_vars_", Sys.Date(),".csv")))

##################################################################################################
#  QC CHECK
##################################################################################################
### CHECK that things are saving correctl - can't view everything on excel. Looks good in R.

check <- TRUE

if(check==TRUE) {
  predictions <- readRDS(file.path(prediction_path, "KP", paste0("predictions_additional_vars_", Sys.Date(),".rda")))
  
  t <- predictions %>%
    group_by(model) %>%
    summarize(n = n(),
              min=min(prediction),
              mean=mean(prediction),
              max=max(prediction))
  
  print(t)
  
  # 1. prediction histograms 
  pacman::p_load(ggplot2)
  
  predictions %>%
    ggplot(., aes(x=prediction)) + 
    facet_wrap(~model, scales = "free") + 
    geom_histogram(bins=30) +
    labs(title = "Prediction Histograms")
  
  
}




