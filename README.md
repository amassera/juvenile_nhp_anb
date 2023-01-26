# juvenile_nhp_anb


Juvenile NHP A-not-B
=======================

The dataset contains behavioural data collected to assess executive function in adolescent and adult macaques. Each subjects has data at two time points. Measures include those reflecting general executive and parameters obtained via the fitting of a computational model of decision making that reflect working memory and inhibitory control. These measures were used as outcome variables in our analyses, which were run to examine how rearing condition and time influenced executive function. 

> A Massera, JJ Bonaiuto, H Rayson, PF Ferrari PF<br>
> **Longitudinal effects of early adversity on macaque executive function: Evidence from computational modeling**<br>

## Description of the data and file structure
All data is presented in csv format. 
Data is organized in long format in the following files: year2.csv; year3.csv; year3_random.csv; model_predictions-run_all_behavs_best_year2; model_predictions-run_all_behavs_best_year3; model_predictions-run_all_behavs_best_year3_random
Data is organized in wide format in the following files: fitted_coefficients-run_5_year2; fitted_coefficients-run_5_year3; fitted_coefficients-run_5_year3_random

Matlab code was utlized to fit the computational model of decision making to the choice behaviour in A not B task for each subject at each time point. 
R code was utilized to preprocess the data, and perform statistical analyses.
  

## Code/Software:

## Top-level matlab model functions
run_model_comparison.m - fit each model to subject behavior
run_models.m - fit selected model to subject behavior
predict_behaviour.m - predict behavior for each subject given fitted model parameter values
compare_models.m - compare models in terms of AIC

## Matlab utility functions
fit_behavior.m - fit a model for each subject (used by run_model_comparison.m, run_models.m)
read_subj_data.m - read and preprocess subject data (used by fit_behavior.m)
fit.m - fit a model to subject data (used by fit_behavior.m)
energy_decision.m - compute the negative log-likelihood for a subject given a set of parameter values (used by fit.m)
predict.m - (used by energy_decision.m, predict_behaviour.m, fit_behavior.m)
params_within_limits.m - check that parameter values are within limits (used by energy_decision.m and fit.m)
predict.m - compute response probabilities for a set of trials given model parameter values (used by predict_behaviour.m, energy_decision.m, fit_behavior.m)
rescorla_td_prediction.m - implements the Rescorla-Wagner RL rule (used by predict.m)

# R data analysis scripts
plot_fit.R - generate predicted response from GLMM
compute_cumulative_score.R - calculate the cumulative score for each subject at each time point
data_analysis_computational_model_parameters.R - Mixed model to compare the fitted model parameters between the two rearing groups across time and plot       (Figure 5 and Figure 6)
data_analysis_parameters_predicting_task_performance.R - linear mixed models for each model parameter and performance measure in the original version of the task across time
data_analysis_task_performance.R - To compare performance on the original A-not-B task between the two rearing groups over time
data_preprocessing_SI.R - pre-processing analyses 
