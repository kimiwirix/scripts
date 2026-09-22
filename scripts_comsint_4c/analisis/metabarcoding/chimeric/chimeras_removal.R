#+ chimeric analysis yields a fasta with all non chimeric sequences, 
#+ thf I have to purify abundance table and only remain non chimeric seqs

library(tibble)
library(readODS)
library(dplyr)
library(Biostrings)

#+ non chimeric seqs obtained from cluster: 
#+ /mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/abundance_table_open/nonchimeric
#+ 
#+ n gets the names of the sequences nonchimeric form the fasta object 

nonchimeric_seqs <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/nonchimeric/dna-sequences.fasta")

n<-names(nonchimeric_seqs)%>%
  as.data.frame()%>%
  rename('.'='strain')


#+ loads freq table and joins it with the nonchimeric names df to only retain
#+ the nonchimeric seqs abundances 

frequency_table <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/feature-table-open.tsv", sep = "\t",
              header = TRUE) 

f <- frequency_table %>%
  rename("X.OTU.ID" = "strain") %>%
  inner_join(n, by = 'strain')


write.table(f, file='C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/feature-table-open-nonchimeric.tsv', 
            quote=FALSE, sep='\t', row.names = FALSE, col.names = TRUE)


