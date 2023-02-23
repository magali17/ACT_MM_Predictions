
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

pacman::p_load( dplyr, readr, lubridate)    

prediction_path <- file.path("output", #"UK Predictions", 
                             #"cohort"
                             "dr0311_grid", "all_predictions"
)

if(!dir.exists(file.path(prediction_path, "KP"))){dir.create(file.path(prediction_path, "KP"), recursive = T)}

##################################################################################################
#  LOAD DATA
##################################################################################################
new_variables <- c("pnc_20_36",
                   "pmdisc_size",
                   "ma200_blue_bc1", "ma200_green_bc1", "ma200_red_bc1", "ma200_uv_bc1"# "ma200_ir_bc1"
                   )
  
predictions0 <- readRDS(file.path(prediction_path, "predictions.rda")) %>%
  filter(variable %in% new_variables)

# var_names <- readRDS(file.path("Output", "keep_vars.rda"))
# predictions0 <- lapply(var_names, 
#                        function(x) {
#                          #read_csv(file.path(prediction_path, x, "predictions.csv"), show_col_types = FALSE)
#                          readRDS(file.path(prediction_path, x, "predictions.rda"))
#                        }) %>%
#   bind_rows() 

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
    #model = paste0("mb_", campaign_id)
  ) %>%
  select(location_id, start_date, end_date, model, 
         #variable,
         prediction)

saveRDS(predictions, file.path(prediction_path, "KP", "predictions_all.rda"))
write_csv(predictions, file.path(prediction_path, "KP", "predictions_all.csv"))

##################################################################################################
#  QC CHECK
##################################################################################################

### CHECK that things are saving correctl - can't view everything on excel. Looks good in R.
test <- read_csv(file.path(prediction_path, "KP", "predictions_all.csv"))
test %>%
  group_by(variable) %>%
  summarize(n = n(),
            min=min(prediction),
            mean=mean(prediction),
            max=max(prediction)
            )

# 1. prediction histograms 
test %>%
  ggplot(., aes(x=prediction)) + 
  facet_wrap(~variable, scales = "free") + 
  geom_histogram(bins=30) +
  labs(title = "Prediction Histograms")


