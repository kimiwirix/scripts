#quiero hacer de las unmatched y nonchimeric seqs un taxonomic profile to know what is the unmatched taxa
#de la table abundances quite todas las unmatched seqs que tengan menos de 50 reads para que no tarde años el cluster en asignar taxonomia
#subir a cluster y convertitra qza y correr silva database  VER: nonchimeric_unmatched_taxa.sh

library(tidyverse)
library(dplyr)
library(Biostrings)

table_abundances<-read.table(file = 'C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/feature-table-open-ensambles-nonchimeric.tsv',
                             sep = "\t", header = TRUE)
nonchimeric_seqs <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/nonchimeric/dna-sequences.fasta")



#de table abundances voy a sacar los headers (ids) (matched y unmatched) que tegan mas de 50 lecturas 
h<-table_abundances%>%
  column_to_rownames(var = 'strain')%>%
  filter(!rowSums(across(everything())) <= 50)%>%
  rownames_to_column(var = 'ids')

ids<-h$ids
  
filtered_seqs <- nonchimeric_seqs[names(nonchimeric_seqs) %in% ids]

writeXStringSet(filepath = 'C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/nonchimeric/nonchimeric-seqs-filtered.fasta',
                  filtered_seqs, width=200000)

