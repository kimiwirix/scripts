#!/bin/bash                                                             
#SBATCH --job-name=nonchimeric_unmatched_taxa_id		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/metabarcoding/ensambles/abundance_table_open/nonchimeric/   	# ruta para guardar todo el output			                                            
#SBATCH --output=Logs_errors/%j.log        	                            # Output file (%j = Job ID)
#SBATCH --error=Logs_errors/%j.error    	                              # Error file
#SBATCH --time=400:00:00           					                            # Time limit (hh:mm:ss)
#SBATCH --partition=defq         					                              # Partition
#SBATCH --nodes=1                 					                            # Number of nodes
#SBATCH --ntasks=1                					                            # Number of tasks (processes)
#SBATCH --cpus-per-task=8         					                            # CPUs per task
#SBATCH --mem=80G                  					                            # Memory per node

#NOTE
#hacer carpeta Logs_errors antes de correr sbatch
#cambiar SBATCH --chdir= a la ruta que vaya a usar para guardar todo el output	

#info slurm: https://support.lavis.unam.mx/documentation/USING-THE-CLUSTERS/job-scheduling-with-slurm/ 

#QUE HACE?
##asigna taxonomia a unmatched sequences. Unmatched seqs ya no tienen chimeras y se eliminaron las secuancias que tenian menos de 50 reads para acelerar proceso

#always while working with conda envs 
eval "$(conda shell.bash hook)" 
#load modules
conda activate qiime2-amplicon-2025.10


#imports fasta witha all the unmatched sequences names to .qza object. fasta ya está limpio ver: R 
qiime tools import \
  --type 'FeatureData[Sequence]' \
  --input-path nonchimeric-seqs-filtered.fasta \
  --output-path unmatched_nonchimeras_filtered.qza


#for classifying nonchimeric and unmatched seqs to know taxonomy and compare how far they are form reference seqs using sklearn classifier
qiime feature-classifier classify-sklearn \
  --i-reads /mnt/data/sur/users/nsaid/4c/metabarcoding/ensambles/abundance_table_open/nonchimeric/unmatched_nonchimeras_filtered.qza \
  --i-classifier SILVA138.2_SSURef_NR99_uniform_classifier_V4-515f-806r.qza \
  --p-n-jobs $SLURM_CPUS_PER_TASK \
  --o-classification nonchimeric-unmatched-taxa-sklearn.qza \
  --verbose


#exportea taxonomy tsv
qiime tools export \
--input-path nonchimeric-unmatched-taxa-sklearn.qza \
--output-path taxa/sklearn


# ## sklearn works better than vsearch    
# #sube base de datos 
# qiime rescript get-silva-data \
#   --p-version '138.2' \
#   --p-target 'SSURef_NR99' \
#   --p-include-species-labels \
#   --o-silva-sequences silva-138.2-seqs.qza \
#   --o-silva-taxonomy silva-138.2-tax.qza

# qiime rescript reverse-transcribe \
#   --i-rna-sequences silva-138.2-seqs.qza \
#   --o-dna-sequences silva-138.2-seqs-dna.qza


# #for classifying nonchimeric and unmatched seqs to know taxonomy and compare how far they are form reference seqs 

# qiime feature-classifier classify-consensus-vsearch \
#   --i-query /mnt/data/sur/users/nsaid/4c/metabarcoding/ensambles/abundance_table_open/nonchimeric/unmatched_nonchimeras_filtered.qza \
#   --i-reference-reads silva-138.2-seqs-dna.qza \
#   --i-reference-taxonomy silva-138.2-tax.qza \
#   --p-threads $SLURM_CPUS_PER_TASK \
#   --p-perc-identity 0.99 \
#   --o-classification nonchimeric-unmatched-taxa.qza \
#   --o-search-results hits-results.qza


# #exportea taxonomy tsv
# qiime tools export \
# --input-path nonchimeric-unmatched-taxa.qza \
# --output-path taxa

