#!/bin/env bash
#$ -N pyani_run
#$ -o ./Logs_errors/$JOB_NAME.log
#$ -e ./Logs_errors/$JOB_NAME.error
#$ -cwd
#$ -S /bin/bash
#$ -l h_rt=20:00:00   # runtime limit of 20 hours
#$ -pe openmp 1         # Specify number of cores
#$ -l h_rss=15G        # Request 15 GB of memory per core


#pyANI

mamba activate pyani_0.3


#creates MD5 file for each sequence and class.txt and label.txt that genearte ids to identify each genome
pyani index -i /mnt/atgc-d3/sur/users/nsaid/4c/genomes 


#creates empty db in main not in genomes directory 
pyani createdb --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db 


#crates nucmer_output directory with all comparisons
pyani anim \
    --i /mnt/atgc-d3/sur/users/nsaid/4c/genomes \
    --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results \
    --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
    --classes /mnt/atgc-d3/sur/users/nsaid/4c/genomes/classes.txt \
    --labels /mnt/atgc-d3/sur/users/nsaid/4c/genomes/labels.txt 


#run_results: complete set of pairwise comparison results for a single run (listed by comparison)
#run_matrixes: comparison results as matrices (percentage identity and coverage, number of aligned bases and “similarity errors”, and a Hadamard matrix of identity multiplied by coverage).
#runs_genomes: the genomes that were analysed in all runs 
pyani report \
    --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
    --formats excel \
    --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results/pyani_report \
    --runs \
    --run_matrices 1 \
    --run_results 1  
    

#run id depending on ids obtanined from pyani report --runs 
pyani plot \
    --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
    --formats png \
    --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results/pyani_plots \
    --run_ids 1 





#NOTAS  
#in pyani report --runs_genomes aparece vacio 
#how to add organism names in matrixes and plots?? 

