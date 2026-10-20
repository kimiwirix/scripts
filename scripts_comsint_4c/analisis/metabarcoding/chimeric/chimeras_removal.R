#+ chimeric analysis yields a fasta with all non chimeric sequences, 
#+ thf I have to purify abundance table and only remain non chimeric seqs
#+ 
#+ Since the CH450 is taken as a chimera (maybe due to low abundances) I am going
#+ to clean only the unmatched reads and the matched I am going to leave as it is
#+ but since I want an updated final table at the end I create a final nonchimeric 
#+ table by uniting unmathced nonchimeric to matched
#+ 
library(tibble)
library(dplyr)
library(Biostrings)

#+ non chimeric seqs obtained from cluster: 
#+ /mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/abundance_table_open/nonchimeric
#+ 
#+ n gets the names of the sequences nonchimeric form the fasta object 

strains_ids<-c('CH23', 'CH29', 'CH90', 'CH99b', 'CH111', 'CH149a', 'CH154a', 'CH161d',  'CH447', 'CH450')

nonchimeric_seqs <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/all_batch0_batches/nonchimeric/dna-sequences.fasta") 

  

n<-names(nonchimeric_seqs)%>%
  as.data.frame() %>%
  rename('.'='strain')%>%
  filter(!(strain %in% strains_ids))


#+ loads freq table matched and unmatched and joins the unmatched with the 
#+ nonchimeric names df to only retain the nonchimeric seqs abundances 
#+ and then joins the matched 

matched <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/matched.tsv", sep = "\t",
              header = TRUE) 
  rename("strain" = "X.OTU.ID")

unmatched <- read.csv("C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/unmatched.tsv", sep = "\t",
                      header = TRUE) %>%
  rename("strain" = "X.OTU.ID")



f <- unmatched %>%
  inner_join(n, by = 'strain') %>%
  bind_rows(matched)



write.table(f, file='C:/Users/natal/Documents/LIIGH/results/results_comsint_4c/analisis/metabarcoding/batches/feature-table-open-nonchimeric.tsv', 
            quote=FALSE, sep='\t', row.names = FALSE, col.names = TRUE)


