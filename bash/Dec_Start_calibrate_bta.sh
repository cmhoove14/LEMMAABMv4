#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
Rscript Analysis/Dec_Start/10-Dec-Start-bta-fit-parallel.R 0.20 0.40 1 1 0.33 3 "data/processed/data_inputs_Dec_Start.rds" "data/processed/input_pars_Dec_start.rds" "data/processed/vax65p_scenario.rds" "data/processed/Dec_Start_Calibrate/"