#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 Rscript Analysis/Calibrate/10-Calibrate-Parallel.R 0.1 0.3 1 1 0.2 3 "data/processed/data_inputs_calibrate.rds" "data/processed/input_pars_calibrate.rds" "data/processed/vax65p_scenario.rds"