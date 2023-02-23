
##################################################################################################
# MODEL EVALUATION
##################################################################################################

# Fn returns RMSE and MSE-based R2 for a given dataset
validation_stats <- function(dt, prediction = "prediction", reference = "value"){
  
  # MSE of predictions
  MSE_pred <- mean((dt[[reference]] - dt[[prediction]])^2)
  # MSE of observations (for R2 denominator)
  MSE_obs <- mean((dt[[reference]] - mean(dt[[reference]]))^2)
  
  RMSE = sqrt(MSE_pred)
  MSE_based_R2 = max(1 - MSE_pred/MSE_obs, 0)
  reg_based_R2 = cor(dt[[reference]], dt[[prediction]], method = "pearson")^2
  
  result <- distinct(dt, variable, out_of_sample) %>%
    mutate(
      no_sites = nrow(dt),
      RMSE = RMSE,
      MSE_based_R2 = MSE_based_R2,
      reg_based_R2 = reg_based_R2
    )
  
  return(result)
  
}



##################################################################################################
# CV function
##################################################################################################
# function returns cross-valited predictions for a given dataset. 
# fn works even if folds don't have all numbers in a sequence (e.g., if too few sites)

do_cv <- function (x, fold_name = "random_fold") {
  #code to make sure this fn works even if folds don't have all numbers in a sequence (e.g., if too few sites)
  k = sort(unique(x[[fold_name]]))
  
  df <- data.frame()
  
  for(f in k) {
    modeling_data0 = filter(x, !!as.symbol(fold_name) != f)
    new_data0 = filter(x, !!as.symbol(fold_name) == f)
    
    temp <- uk_pls(new_data = new_data0, modeling_data = modeling_data0) #%>% st_drop_geometry() 
    df <- rbind(df, temp)
  }
  
  return(df)
}



##################################################################################################
# CREATE RANDOM VALIDATION FOLDS FOR TRAINING SET
##################################################################################################
#k-folds for CV
k <- 10

add_random_fold <- function(df, k.=k) {
  # reproducibility across studies if using same sites
  set.seed(1)
  
  folds <- df %>%
    #make sure different variables, annual estimates, etc. receive same fold designation
    distinct(location) %>%
    arrange(location) %>%
    
    mutate(random_fold = sample(1:k.,size = nrow(.), replace = T ))
  
  result <- left_join(folds, df, by="location")
  
  return(result)
}


###########################################################################################
# UK-PLS
###########################################################################################
# default settings for functions
cov_names <- readRDS(file.path("data", "output", "modeling_covariates.rda"))
pls_comp_n <- readRDS(file.path("data", "output", "objects", "pls_comp_n.rda"))
lat_long_crs <- 4326  

# modeling_data=group_split(annual, variable)[[1]] %>% filter(random_fold !=1)
# new_data = group_split(annual, variable)[[1]] %>% filter(random_fold ==1)

uk_pls <- function(modeling_data, # data for fitting pls-uk models
         new_data, #prediction locations
         cov_names. = cov_names,  #covariates to be used in modeling
         pls_comp_n. = pls_comp_n, #pls components to use
         fn_result = "predictions", #can be: "predictions" or "models"; return the model predictions or the model fit information
         # optional: selects the best variogram fit, unless otherwise stated
         var_choice = "" #Exp
) {
  
  #lambert projection for UK model
  lambert_proj <- "+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"
  ############################################################################################################
  # fit PLS model to estimate fewer components from geocovariates
  pls_model <- plsr(as.formula(paste('value ~', paste(cov_names., collapse = "+"))),
                    data = modeling_data, ncomp = pls_comp_n., scale=T, center=T)
  
  #extract compoent scores for UK
  modeling_data_scores <- predict(pls_model, type = "scores") %>% data.frame() %>%  
    # add location & value info
    cbind(data.frame(select(modeling_data, -all_of(cov_names.)))) %>%
    #convert to sf geom 
    st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs, remove=F)
  
  new_data_scores <- predict(pls_model, type = "scores", newdata = new_data) %>% data.frame() %>%  
    # add location & value info
    cbind(data.frame(select(new_data, -all_of(cov_names.)))) %>%
    #convert back to sf. geom is dropped otherwise
    st_as_sf(coords = c('longitude', 'latitude'), crs= lat_long_crs, remove=F)
  
  ############################################################################################################
  # fit UK models & predict at new locations
  
  # UK formula 
  uk_formula <- as.formula(paste("value ~", paste0("Comp.", 1:pls_comp_n., collapse = "+")))
  
  # estimate the variogram model: fit a variogram model, offering to the function several different model options (exponential, spherical, and Matern):
  # using lambert coordinates b/c vertical/horizontal units represent the same jump
  # the default distance in gstat is 1/3 of the maximum distance (use cutoff option to change this)
  v_uk <- variogram(uk_formula, st_transform(modeling_data_scores, lambert_proj) )
  
  
  if(var_choice == "") {
    #select the best fitting variogram
    m_uk <- fit.variogram(v_uk, vgm(c("Exp", "Sph", "Mat")))
  } else {
    # or select a specific variogram if it's not left blank
    m_uk <- fit.variogram(v_uk, vgm(var_choice))
  }
  
  #make sure Exp/Sph range estimate is at least 0 when little/no correlation in the data 
  m_uk$range[2] <- max(m_uk$range[2], 1)
  
  # fit UK to the modeling data and predict at the new data locations
  uk_model <- krige(formula = uk_formula, st_transform(modeling_data_scores, lambert_proj), 
                    newdata = st_transform(new_data_scores, lambert_proj), 
                    model = m_uk)
  
  #save predictions
  predictions <- select(new_data, -all_of(cov_names.)) %>%
    mutate(prediction = uk_model$var1.pred)
  
  # return the desired output: either the predictions or the modeling specifications
  if(fn_result == "predictions") {return(predictions)}
  if(fn_result == "models") {
    result = list(
      variable = first(modeling_data$variable),
      pls_model = pls_model, 
      variogram_model = m_uk
    )
    return(result)
  }
  
}


###########################################################################################
# GENERATE NEW COVARIATES FOR THE DATASET
###########################################################################################
# created some new proximity variables  
# log transform land proximity variables (e.g., distance to roadways)

combine_a23_ll <- function(df) {
  #find buffers for a2-3 length variables
  buffers <- str_subset(names(df), "ll_a[2:3]") %>% str_extract("s[0:9].*")
  
  #for each buffer, calculate sum of a2+a3 length
  for (i in seq_along(buffers)) {
    old_vars <- paste0(c("ll_a2_", "ll_a3_"), buffers[i])
    new_var <- paste0("ll_a23_", buffers[i])
    
    df[new_var] <- apply(df[old_vars], 1, sum)
  }
  return(df)
}

generate_new_vars <- function(df) {
  # for the NO2 covariate, use the average levels from several available years
  no2_behr_vars <- c("no2_behr_2005","no2_behr_2006", "no2_behr_2007")
  
  df <- df %>%
    rowwise() %>%
    mutate(m_to_a123 = min(m_to_a1, m_to_a2, m_to_a3),
           m_to_a23 = min(m_to_a2, m_to_a3),
           no2_behr = mean(!!as.symbol(no2_behr_vars))
    ) %>%
    ungroup() %>%
    #make min distance 1 m before log transforming
    mutate_at(vars(starts_with("m_to_")), ~ifelse(.==0, 1, .) %>% log(.)) %>%
    rename_at(vars(starts_with("m_to_")), ~gsub("m_to_", "log_m_to_", .)) %>%
    # calculate sum of a2 and a3 roads in each buffer
    combine_a23_ll()
}

