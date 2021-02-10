#!/bin/bash                        #-- what is the language of this shell
#                                  #-- Any line that starts with #$ is an instruction to SGE
#$ -S /bin/bash                    #-- the shell for the job
#$ -cwd                            #-- tell the job that it should start in your working directory
#$ -o data/outputs/Logs2/FitLogs/NLLjob-$TASK_ID.stdout #-- out directory
#$ -e data/outputs/Logs2/FitLogs/NLLjob-$TASK_ID.stderr #-- error directory
#$ -j y                            #-- tell the system that the STDERR and STDOUT should be joined
#$ -l mem_free=1G                  #-- submits on nodes with enough free memory (required)
#$ -l scratch=10G                  #-- SGE resources (home and scratch disks)
#$ -l h_rt=00:05:00                #-- runtime limit 
#$ -t 1-1000                       #-- array job
                                   #-- tasks if desired (see Tips section on this page)
module load CBI r
Rscript Analysis/Calibrate2/20-2-Calibration-Fit-Scores-NLL-Wynton.R $SGE_TASK_ID 