#!/bin/bash                                                             
#SBATCH --job-name=orthofinder		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/genome_annot/prokka_annotation/faa_prokka_files   	# ruta para guardar todo el output			                                            
#SBATCH --output=Logs_errors/%j.log        	                            # Output file (%j = Job ID)
#SBATCH --error=Logs_errors/%j.error    	                              # Error file
#SBATCH --time=500:00:00           					                            # Time limit (hh:mm:ss)
#SBATCH --partition=defq         					                              # Partition
#SBATCH --nodes=1                 					                            # Number of nodes
#SBATCH --ntasks=1                					                            # Number of tasks (processes)
#SBATCH --cpus-per-task=20         					                            # CPUs per task
#SBATCH --mem=80G                  					                            # Memory per node

#NOTE
#hacer carpeta Logs_errors antes de correr sbatch
#cambiar SBATCH --chdir= a la ruta que vaya a usar para guardar todo el output	

#info slurm: https://support.lavis.unam.mx/documentation/USING-THE-CLUSTERS/job-scheduling-with-slurm/ 

  

#always while working with conda envs 
eval "$(conda shell.bash hook)" 
#load modules
conda activate orthofinder

# info ortho: https://davidemms.github.io/orthofinder_tutorials/running-an-example-orthofinder-analysis.html
# will find protein sequences that are ortologs, meaning different species, same function and structure
# it will find othologous proteins, and the ones that share the same 10 strains I will align the nucleotide sequence
# (obtained in prokka .fna) in MUSCLE to see which ones share  fragments to design the primers on 

orthofinder -f /mnt/data/sur/users/nsaid/4c/genome_annot/prokka_annotation/faa_prokka_files -t 20 -a 20 -d


