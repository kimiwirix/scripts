#!/bin/bash                                                             
#SBATCH --job-name=annotation		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/genome_annot   	# ruta para guardar todo el output			                                            
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
conda activate prokka

# info prokka: https://angus.readthedocs.io/en/2017/prokka_genome_annotation.html
# prokka is software to annotate genomes and produces sevaral outputs, what I need: 
# .tsv with general information of predicted genes
# .ffn with gene sequence 
# .gbk with gene traslation and genome location 

prokka Bacillus_altitudinis_23_x.fasta --outdir prokka_annotation/Bacillus_altitudinis_23_x --prefix Bacillus_altitudinis_23_x & 
prokka Bacillus_atrophaeus_90_x.fasta  --outdir prokka_annotation/Bacillus_atrophaeus_90_x --prefix Bacillus_atrophaeus_90_x &
prokka Bacillus_infantis_161_d.fasta  --outdir prokka_annotation/Bacillus_infantis_161_d --prefix Bacillus_infantis_161_d &
prokka Bacillus_thuringiensis_111_x.fasta --outdir prokka_annotation/Bacillus_thuringiensis_111_x --prefix Bacillus_thuringiensis_111_x & 
prokka Corynebacterium_sp_29_x.fasta  --outdir prokka_annotation/Corynebacterium_sp_29_x --prefix Corynebacterium_sp_29_x &
prokka Metabacillus_indicus_450_x.fasta  --outdir prokka_annotation/Metabacillus_indicus_450_x --prefix Metabacillus_indicus_450_x &
prokka Micrococcus_luteus_149_a.fasta --outdir prokka_annotation/Micrococcus_luteus_149_a --prefix Micrococcus_luteus_149_a & 
prokka Priestia_megaterium_447_x.fasta  --outdir prokka_annotation/Priestia_megaterium_447_x --prefix Priestia_megaterium_447_x &
prokka Staphylococcus_arlettae_99_b.fasta  --outdir prokka_annotation/Staphylococcus_arlettae_99_b --prefix Staphylococcus_arlettae_99_b &
prokka Staphylococcus_shinii_154_a.fasta --outdir prokka_annotation/Staphylococcus_shinii_154_a --prefix Staphylococcus_shinii_154_a 

wait 


# this results will be used for finding monocopy genes (rpob, ftsz) and their location 
# to create primers and use as comparison for qpcr, to know how many 16S copies each 
# genome has 