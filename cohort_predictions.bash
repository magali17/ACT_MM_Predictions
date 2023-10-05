script generates cohort predictions & saves them in the correct format for KPRI
#bash B_cohort_predictions.bash

#############################################################################################
NanoScan bins
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_10_100.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_10_100 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_11.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_11.5 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_15.4.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_15.4 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_20.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_20.5 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_27.4.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_27.4 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_36.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_36.5 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_48.7.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_48.7 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_64.9.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_64.9 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_86.6.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_86.6 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_115.5.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_115.5 rda
Rscript 1_make_predictions.R data/output/specific_pollutants/annual_avg_ns_154.0.rda data/dr0357_cohort_covar_20220404_in_mm_area.rda output/cohort/ns_154.0 rda

#############################################################################################

# Rscript 6_clean_predictions.R

#############################################################################################

echo "DONE MAKING COHORT PREDICTIONS"
