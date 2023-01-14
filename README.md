# ACT_MM_Predictions
make predictions from ACT mobile monitoring data

# Prep (do once)
1. download this code repository 
2. add the data/original folder to the code repository (includes annual.rda and monitoring_land_zero_water_shp.rda)
3. run the prep_data.R script to generate additional files

## Making Predicitons
4. run the 1_make_predictions.R script a terminal: Rscript 1_make_predictions.R  <modeling_data_path> <covariate_file_path> <prediction_directory> <prediction_file_format>

