#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -m beas
#$ -M choover@berkeley.edu
# 

export OMP_NUM_THREADS=1 
Rscript Analysis/Calibrate/12-Calibrate-Parallel.R 289 360 "data/processed/data_inputs_calibrate.rds" "data/processed/input_pars_calibrate.rds" "data/processed/vax65p_scenario.rds" "data/outputs/LHS_Calibration/" "T" "S" "F" "F" "F"