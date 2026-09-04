#!/bin/bash                                                             
#SBATCH --job-name=alignmentsmuscle		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/genome_annot/prokka_annotation/faa_prokka_files/OrthoFinder/Results_Jul02/Orthogroup_FASTAs/Orthogroup_FASTAs   	# ruta para guardar todo el output			                                            
#SBATCH --output=alignments_muscle/Logs_errors/%j.log        	                            # Output file (%j = Job ID)
#SBATCH --error=alignments_muscle/Logs_errors/%j.error    	                              # Error file
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

  
 
#load modules
module load muscle/5.3




#Takes all the previoulsy made fastas con genes ortologos (327 fastas con 10 genes de cada bact c/u)
#and makes the alignment and stores it in alignments directory with extension .afa

for f in *.fasta; do
    muscle -align "$f" -output "alignments_muscle/${f%.fasta}.afa"
done
