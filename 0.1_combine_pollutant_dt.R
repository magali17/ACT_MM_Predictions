
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

pacman::p_load(tidyverse, lubridate)    

#source("file_paths.R")
#source(file.path(hei3a_fp, "functions.R"))
source("functions_hei3a.R")

if(!dir.exists(file.path("data", "output"))) {dir.create(file.path("data", "output"), recursive = T)}

##################################################################################################
# combine datasets & create new size bins
##################################################################################################
## original, non-winsorized data
dt1 <- readRDS(file.path("data", "input", "stop_data.rda")) %>%
  select(-c(primary_instrument, instrument_id, mean_value)) %>%
  rename(value = median_value) %>%
  mutate(# for merging later
    time = ymd_hms(time, tz = "PST8PDT"))

# calculate p-trak screened & no screen difference
dt1 <- dt1 %>%
  filter(variable %in% c("pnc_noscreen", "pnc_screen")) %>% 
  pivot_wider(names_from = "variable", values_from = "value") %>%
  mutate(pnc_20_36 = pnc_noscreen - pnc_screen) %>%  
  select(-c(pnc_noscreen, pnc_screen)) %>%
  drop_na() %>%
  pivot_longer(cols = "pnc_20_36", names_to = "variable") %>%
  bind_rows(dt1) #%>% arrange(time)  

dt1_vars <- unique(dt1$variable)

# NS bins (in dt2) have diff time stamps
correct_times <- distinct(dt1, time, stop_id)

##################################################################################################
# new variables (Dustin)
dt2 <- read_csv(file.path("data", "input", "tr0090_averaged_stops.csv")) %>%
  select(-c(primary_instrument, mean_value)) %>%
  rename(value = median_value) %>%
  
  # 2 largest bins have missingness > 95% of the time. 3rd largest in has missingness 53% of the time
  #filter(!variable %in% c("205.4", "273.8", "365.2")) %>%
  
  mutate(variable = ifelse(grepl("SCAN", instrument_id), paste0("ns_", variable), variable),
         variable = ifelse((!grepl("ns_", variable) | variable == "ns_total.conc"),
                           gsub("\\.", "_", variable), variable),
         # for merging later
         time = ymd_hms(time, tz = "PST8PDT")) %>% 
  select(-instrument_id) %>%
  #these are in dt1 already
  filter(!variable %in% dt1_vars) %>%
  select(-time)  



# fix bin times
dt2 <-left_join(correct_times, dt2, by="stop_id") %>%
  drop_na(value)


# add 10_100 bin counts
small_bins <- paste0("ns_", c(15.4, 20.5, 27.4, 36.5, 48.7, 64.9, 86.6))

small_ufp <- dt2 %>%
  filter(variable %in% small_bins) %>% 
  group_by(time, stop_id, runname, date, location) %>%
  summarize(value = sum(value)) %>%
  mutate(variable = "ns_10_100") %>%
  ungroup()

dt2 <- rbind(dt2, small_ufp)

##################################################################################################
# merge data & winsorize median values, add temporal variables
dt_merge <- rbind(dt1, dt2) %>%
  group_by(variable, location) %>%
  winsorize_fn(value = "value") %>%
  ungroup() %>%  
  select(-c(value)) %>%  
  #use winsorized values for all subsequent data description
  rename(value=win_value) %>%
  ungroup() %>%
  
  # add temporal variables
  mutate(day = wday(time,label = T, week_start = 1),
         hour = hour(time)) %>%
  add_season(.date_var = "date") %>%
  arrange(time) 

saveRDS(dt_merge, file.path("data", "output", "winsorized_medians.rda"))
##################################################################################################
# calculate annual averages
##################################################################################################
annual <- dt_merge %>%
  group_by(variable, location) %>%
  summarize( value = mean(value)) 

saveRDS(annual, file.path("data", "Output", "annual_avgs.rda"))

##################################################################################################
# cleaned covariates used in modeling for all 309 sites
##################################################################################################
fp <- file.path("~", "OneDrive - UW", "Documents", "Post Doc", "Study Projects", "ACT TRAP MM", "ACT HEI Supp", "act_hei_aim1a", "Output")

cov_train <- readRDS(file.path(fp, "mm_cov_train_set.rda"))
cov_test <- readRDS(file.path(fp, "mm_cov_test_set.rda")) %>%
  select(names(cov_train))

modeling_covariates <- rbind(cov_train, cov_test)

saveRDS(modeling_covariates, file.path("data", "Output", "modeling_geocovariates.rda"))



