#!/bin/bash                        #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                    #-- the shell for the job
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -o data/outputs/Logs2/job-$TASK_ID.stdout #-- out directory
#$ -e data/outputs/Logs2/job-$TASK_ID.stderr #-- error directory
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=5G                  #-- submits on nodes with enough free memory (required)
#$ -l scratch=70G                  #-- SGE resources (home and scratch disks)
#$ -l h_rt=04:00:00                #-- runtime limit (see above; this requests 24 hours)
#$ -t 1-1000                       #-- array job
                                   #-- tasks if desired (see Tips section on this page)
module load CBI r
Rscript Analysis/Calibrate2/12-Calibrate-Wynton.R $SGE_TASK_ID "data/processed/data_inputs_calibrate.rds" "data/processed/input_pars_calibrate.rds" "data/processed/vax65p_scenario.rds" "data/outputs/Calibration2_Outputs/" "T" "S" "F" "F" "F"