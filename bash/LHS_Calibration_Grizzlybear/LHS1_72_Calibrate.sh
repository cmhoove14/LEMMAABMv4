#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -m beas
#$ -M choover@berkeley.edu
# 

Rscript Analysis/Calibrate/12-Calibrate-Parallel.R 1 72 "data/processed/data_inputs_calibrate.rds" "data/processed/input_pars_calibrate.rds" "data/processed/vax65p_scenario.rds" "data/outputs/Calibration_Sims/" "T" "S" "F" "F" "F"