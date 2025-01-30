#!/bin/env bash
#$ -N raw_16Sanalysis_all
#$ -o ./Logs_errors/$JOB_NAME.log
#$ -e ./Logs_errors/$JOB_NAME.error
#$ -cwd
#$ -S /bin/bash
#$ -l h_rt=20:00:00   # runtime limit of 10 hours
#$ -pe openmp 1         # Specify number of cores
#$ -l h_rss=15G        # Request 15 GB of memory per core

# hacer carpeta logs errors antes de correr qsub

#load modules

#Code