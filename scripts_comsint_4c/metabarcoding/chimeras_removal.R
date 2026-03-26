##chimeric analysis yields a fasta with all non chimeric sequences, thf I have to purify abundance table and only remain non chimeric seqs

library(tibble)
library(readODS)
library(dplyr)
library(Biostrings)

nonchimeric_seqs <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/nonchimeric/dna-sequences-wo-primers.fasta")

n<-names(nonchimeric_seqs)%>%
  as.data.frame()%>%
  rename('.'='strain')


frequency_table<-read.table(file ='C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/feature-table-open-ensambles-wo-primers.tsv', 
               sep = "\t", header = TRUE, comment.char = "", check.names = FALSE , skip = 1)%>%
  rename(`#OTU ID`='strain')


f<-inner_join(frequency_table,n, by = 'strain')

write.table(f, file='C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batch_0/feature-table-open-ensambles-nonchimeric-wo-primers.tsv', 
            quote=FALSE, sep='\t', row.names = FALSE, col.names = TRUE)



