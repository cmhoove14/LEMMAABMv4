#!/bin/bash                        #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                    #-- the shell for the job
#$ -o [dir]                        #-- output directory (fill in)
#$ -e [dir]                        #-- error directory (fill in)
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=5G                  #-- submits on nodes with enough free memory (required)
#$ -l scratch=80G                  #-- SGE resources (home and scratch disks)
#$ -l h_rt=48:00:00                #-- runtime limit (see above; this requests 24 hours)
##$ -t 1-10                        #-- remove first '#' to specify the number of
                                   #-- tasks if desired (see Tips section on this page)

Rscript Analysis/Calibrate/12-Calibrate-Parallel.R 1 64 "data/processed/data_inputs_calibrate.rds" "data/processed/input_pars_calibrate.rds" "data/processed/vax65p_scenario.rds" "data/outputs/Calibration_Outputs/" "T" "S" "F" "F" "F"