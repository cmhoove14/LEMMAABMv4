#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
Rscript Analysis/Dec_Start/12-Dec-Start-vax-runs-parallel.R 0.3 1 1 0.33 1 "data/processed/data_inputs_Dec_Start.rds" "data/processed/input_pars_Dec_start.rds" "data/processed/vaxess_scenario.rds" "data/processed/Dec_start_vaxessS/" "T" "S" "S" "F" "T"