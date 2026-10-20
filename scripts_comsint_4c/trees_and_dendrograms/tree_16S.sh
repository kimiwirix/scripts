#!/bin/bash                                                             
#SBATCH --job-name=alignmentsmuscle		                                          # Job name
#SBATCH --chdir=/mnt/data/sur/users/nsaid/4c/metabarcoding/pruebas/16S_tree   	# ruta para guardar todo el output			                                            
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



# A MSA and tree of the 16S sequences was attempted however there was long branch attraction and so a bact120 tree of genomes of reference
# was contructed using surs script on cluster: /mnt/data/sur/users/nsaid/4c/trees/commands_phylo_genomes/
 
#load modules
module load muscle/5.3


#Takes the fasta with the refs and makes the alignment 
# alidnment can be .afa or .fasta 

muscle -align reference_seqs_sangercontigR_trimmed_wo_primers.fasta \
       -output refs_16S_aligned.fasta



# Makes the tree from the alignments with qiime

#always while working with conda envs 
eval "$(conda shell.bash hook)" 
#load modules
conda activate qiime2-amplicon-2025.10

qiime tools import \
  --type 'FeatureData[AlignedSequence]' \
  --input-path refs_16S_aligned.fasta \
  --output-path references-aligned.qza


qiime phylogeny fasttree \
  --i-alignment references-aligned.qza \
  --o-tree references-tree.qza


qiime tools export \
  --input-path references-tree.qza \
  --output-path references-tree



# Makes the tree from the alignments with iq.tree

module load iq-tree/3.0.1
iqtree -s references_aligned.fasta 