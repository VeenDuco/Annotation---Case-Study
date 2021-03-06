# DESCRIPTION:
# From this script we source all the relevant SBC simulations.
# Each sourced script is a stand alone file that could be run on it's own
# All models are called within the scripts themselves and results are saved
# within the scripts themselves. In this script we call the simulations and
# postprocess results to create a shared overview between the simulations.


# Scripts structure implies story line:
# We want to do two things: 
#   - Get labels from annotations: using probibalistic model instead of maj vote
#   - Use labels to estiamte predictor coefficients.
# 
# We will compare two different approaches to obtain both goals:
#   1. Get the labels and then use these labels in logtistic regression
#   2. Do both simultaniously and estimate a joint model.
#
#
# For this case study the question of interest is the following:
#   - Is the coverage of these methods appropriate? 
#
# To answer this question we will use Simulation Based Calibration (SBC).
# To investigate this question systematically we first look if the seperate
# parts of the model are well calibrated;the probibalistic labeling model and
# the linear regression.
# Thereafter we look at the calibration of the two-step model (1.) and the 
# joint model (2.).


start_time_lr <- Sys.time()
source("SIMULATIONS/SCRIPTS/SBC_logistic_regression_second_step.r")
end_time_lr <- Sys.time()
# source("SIMULATIONS/SCRIPTS/SBC_logistic_regression_second_step_N_equals_I_data.r")
lr.time <- ((end_time_lr - start_time_lr) / 3000) * 3

start_time_two_step <- Sys.time()
source("SIMULATIONS/SCRIPTS/SBC_raykar_two_step_model.r")
end_time_two_step <- Sys.time()
two_step.time <- ((end_time_two_step - start_time_two_step) / 1000) * 3

start_time_raykar_full <- Sys.time()
source("SIMULATIONS/SCRIPTS/SBC_raykar_full_model.r")
end_time_raykar_full <- Sys.time()
raykar.time <- ((end_time_raykar_full - start_time_raykar_full) / 1000) * 3

start_time_DS <- Sys.time()
source("SIMULATIONS/SCRIPTS/SBC_raykar_reduced_to_Dawid_Skene.r")
end_time_DS <- Sys.time()
# time per simulation with 19(20) bins
DS.time <- ((end_time_DS - start_time_DS) / 1000) * 3

writeLines(capture.output(sessionInfo()), "SIMULATIONS/sessionInfo.txt")
