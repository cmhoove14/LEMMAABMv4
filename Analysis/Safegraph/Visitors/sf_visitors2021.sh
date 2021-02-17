#!/bin/bash 
# 
#$ -cwd 
#$ -V 
#$ -j y 
#$ -S /bin/bash 
#$ -M choover@berkeley.edu
#$ -m beas
# 
 
mpirun -n 1 Rscript Analysis/Safegraph/Visitors/safegraph_sf_visitors.R 2021 ../aws_downloads