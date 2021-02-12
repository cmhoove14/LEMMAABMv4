#!/bin/bash                        #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                    #-- the shell for the job
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -o data/outputs/Logs/REPjob-$TASK_ID.stdout #-- out directory
#$ -e data/outputs/Logs/REPjob-$TASK_ID.stderr #-- error directory
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=5G                  #-- submits on nodes with enough free memory (required)
#$ -l scratch=10G                  #-- SGE resources (home and scratch disks)
#$ -l h_rt=04:00:00                #-- runtime limit 
#$ -t 1-110                        #-- array job
                                   #-- tasks if desired (see Tips section on this page)
module load CBI r
Rscript Analysis/Calibrate/24-Best-Sims-Replicates.R $SGE_TASK_ID "data/outputs/Calibration_Reruns/"