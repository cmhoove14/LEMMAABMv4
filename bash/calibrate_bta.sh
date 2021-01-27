#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 Rscript bash/bash_run_parallel.R 0.25 1 1 0.33 "data/processed/data_inputs.rds" "data/processed/input_pars.rds" "data/processed/vax65p_scenario.rds"