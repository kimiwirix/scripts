#!/bin/bash                                                             
#SBATCH --job-name=pyani		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/pyani   	# ruta para guardar todo el output			                                            
#SBATCH --output=Logs_errors/%j.log        	                            # Output file (%j = Job ID)
#SBATCH --error=Logs_errors/%j.error    	                              # Error file
#SBATCH --time=50:00:00           					                            # Time limit (hh:mm:ss)
#SBATCH --partition=defq         					                              # Partition
#SBATCH --nodes=1                 					                            # Number of nodes
#SBATCH --ntasks=1                					                            # Number of tasks (processes)
#SBATCH --cpus-per-task=2         					                            # CPUs per task
#SBATCH --mem=80G                  					                            # Memory per node

#NOTE
#hacer carpeta Logs_errors antes de correr sbatch
#cambiar SBATCH --chdir= a la ruta que vaya a usar para guardar todo el output	

#info slurm: https://support.lavis.unam.mx/documentation/USING-THE-CLUSTERS/job-scheduling-with-slurm/ 




#THIS IS WITH NEW VERSON OF PYANI 0.3
#put all genomes .fasta or .fna in genomes folder

#always while working with conda envs 
eval "$(conda shell.bash hook)" 
#load modules
conda activate pyani_env

#output folder MUST NOT exist
average_nucleotide_identity.py \
 -i /mnt/data/sur/users/nsaid/4c/pyani/genomes \
 -o ani_output \
 -m ANIm \
 -g 




#THIS IS WITH OLD VERSION OF PYANI 
#create all directories: genomes, pyani_results, pyani_report, pyani_plots

# module load mamba/main 
# mamba activate /mnt/atgc-d3/sur/modules/pkgs/mamba/main/envs/pyani_0.3
# #creates MD5 file for each sequence and class.txt and label.txt that genearte ids to identify each genome
# pyani index -i /mnt/atgc-d3/sur/users/nsaid/4c/genomes 
# #creates empty db in main not in genomes directory 
# pyani createdb --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db 
# #crates nucmer_output directory with all comparisons
# pyani anim \
#     --i /mnt/atgc-d3/sur/users/nsaid/4c/genomes \
#     --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results \
#     --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
#     --classes /mnt/atgc-d3/sur/users/nsaid/4c/genomes/classes.txt \
#     --labels /mnt/atgc-d3/sur/users/nsaid/4c/genomes/labels.txt 
# #run_results: complete set of pairwise comparison results for a single run (listed by comparison)
# #run_matrixes: comparison results as matrices (percentage identity and coverage, number of aligned bases and “similarity errors”, and a Hadamard matrix of identity multiplied by coverage).
# #runs_genomes: the genomes that were analysed in all runs 
# pyani report \
#     --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
#     --formats excel \
#     --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results/pyani_report \
#     --runs \
#     --run_matrices 1 \
#     --run_results 1  
# #run id depending on ids obtanined from pyani report --runs 
# pyani plot \
#     --dbpath /mnt/atgc-d3/sur/users/nsaid/4c/db \
#     --formats png \
#     --o /mnt/atgc-d3/sur/users/nsaid/4c/pyani_results/pyani_plots \
#     --run_ids 1 


