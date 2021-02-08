#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -m beas
#$ -M choover@berkeley.edu
# 

Rscript Analysis/Calibrate/20-Calibration-Fit_Scores.R