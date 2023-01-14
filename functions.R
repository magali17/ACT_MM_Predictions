
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
###########################################################################################
# UK-PLS
###########################################################################################

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
    st_as_sf()
  
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
    mutate(prediction = uk_model$var1.pred#,
           
           #TEST 
           #var1.var = uk_model$var1.var
    )
  
  #return(result)
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