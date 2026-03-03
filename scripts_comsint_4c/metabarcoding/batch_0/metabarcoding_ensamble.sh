#!/bin/bash                                                             
#SBATCH --job-name=ensamble		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/metabarcoding/ensambles   	# ruta para guardar todo el output			                                            
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




#PRIOR	                                            
#cambiar nombre de manifest y de references.txt antes de correr programa 
#cambiar nombre de outputs finales .tsv y .biom dependiendo del análisis
#Hacer sobre raw data files (ej:NS00173B.raw_2.fastq)
   

#always while working with conda envs 
eval "$(conda shell.bash hook)" 
#load modules
conda activate qiime2-amplicon-2025.10

#imports references 
qiime tools import \
  --type 'FeatureData[Sequence]' \
  --input-path reference_seqs_sangercontigR_trimmed_wo_primers.txt \
  --output-path reference_seqs.qza

# 6. Muestras: importea sequencias a artifacto .qza
# dejar el .gz en manifest y en muestras 
# hacer manifest con echo -e "" > manifestname.tsv and >> para append y no overwrite
qiime tools import \
  --type 'SampleData[PairedEndSequencesWithQuality]' \
  --input-path manifest.tsv \
  --output-path paired-end-demux.qza \
  --input-format PairedEndFastqManifestPhred33V2


# 7. Muestras: une secuencias R y F
#in latest versions join-pairs changed to merge-pairs
qiime vsearch merge-pairs \
 --i-demultiplexed-seqs paired-end-demux.qza \
 --o-merged-sequences paired-end-merged.qza \
 --o-unmerged-sequences unpaired-end-merged.qza


# 8. Filtro por q score, trimmea los ends que esten muy degradados 
qiime quality-filter q-score \
  --i-demux paired-end-merged.qza \
  --o-filtered-sequences demux-filtered.qza \
  --o-filter-stats demux-filter-stats.qza

# 9. Agrupa las secuencias repetidas para que la comparcion con las referencias no sea tan larga
qiime vsearch dereplicate-sequences \
  --i-sequences demux-filtered.qza \
  --o-dereplicated-sequences dereplicated-seqs.qza \
  --o-dereplicated-table dereplicated-table.qza

# comparacion con secuencias
# 10. open 
qiime vsearch cluster-features-open-reference \
  --i-table dereplicated-table.qza \
  --i-sequences dereplicated-seqs.qza \
  --i-reference-sequences reference_seqs.qza \
  --p-perc-identity 0.99 \
  --o-clustered-table table-cr-85.qza \
  --o-clustered-sequences rep-seqs-cr-85.qza \
  --o-new-reference-sequences new-references-cr-85.qza

# # 10. closed 
# qiime vsearch cluster-features-closed-reference \
#   --i-table dereplicated-table.qza \
#   --i-sequences dereplicated-seqs.qza \
#   --i-reference-sequences reference_seqs.qza \
#   --p-perc-identity 0.97 \
#   --o-clustered-table table-cr-85.qza \
#   --o-clustered-sequences rep-seqs-cr-85.qza \
#   --o-unmatched-sequences unmatched-cr-85.qza

# 11. exportea la tabla de frecuencias 
qiime tools export \
--input-path table-cr-85.qza \
--output-path abundance_table_open

# 12. convierte tabla a tsv
# cambiar nombre output
biom convert -i abundance_table_open/feature-table.biom \
-o abundance_table_open/feature-table-open-ensambles.tsv --to-tsv

#convertirunmatched sequences a directorio con fasta 
qiime tools export \
    --input-path new-references-cr-85.qza \
    --output-path unmatched-sequences

#workflow to get nonchimeric sequence titles
qiime vsearch uchime-denovo \
  --i-sequences new-references-cr-85.qza \
  --i-table table-cr-85.qza \
  --o-chimeras chimeras.qza \
  --o-nonchimeras nonchimeras.qza \
  --o-stats chimera-stats.qza

qiime tools export \
  --input-path nonchimeras.qza \
  --output-path abundance_table_open/nonchimeric

#For knowing what kind of artifact is the file
  #qiime tools peek reference_seqs.qza