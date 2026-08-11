#+ PROKKA + ORTHOFINDER + MUSCLE



#+ de la carpeta /mnt/data/sur/users/nsaid/4c/genome_annot/prokka_annotation/faa_prokka_files/OrthoFinder/Results_Jul02/Orthogroups
#+ sacar dos archivos: Orthogroups.tsv y Orthogroups.GeneCount.tsv. 
#+ En GeneCount.tsv, sacar las filas que tengan 1 gen otrólogo en cada columna (cepas), de esa selecciópn sacar de orth.tsv el id que contiene 
#+ los nombres de las secuencias en el genoma anotado. Cuando ya tenga los nombres de los genes, meterlos a MUSCLE para 
#+ hacer alineamiento y ver que grupo de genes se alinean mejor para sacar los primers


library(Biostrings)
library(dplyr)


#+ Tabla con los ids en filas y las cepas en columnas. Cada id tiene las secuencias que son ortólogas entre ellas 
#+ y yo las filtro por las que en total son 10 pero tienen una secuencia por cepa porque esas son monocopia

gene_count <- read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/orthofinder/Orthogroups.GeneCount.tsv", 
                         header = TRUE,  sep='\t' )%>%
  filter(Total==10, if_all(2:11, ~. == 1 ))



#+ Guardo los ids de los nombres de las proteinas que estan presentes en cada cepa una sola vez 

orth_id <- gene_count$Orthogroup



#+ Aquí es una tabla que me dice el id y el nombre de la proteina por cepa

orth_tsv <-  read.table(file = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/orthofinder/Orthogroups.tsv", 
                        header = TRUE,  sep='\t' )


# Aquí extraingo solo las filas que tengan los ids de las proteinas que estan en todas iguales pero solo una copia 

s <- orth_tsv %>%
  filter (Orthogroup %in% orth_id) %>%
  pivot_longer(cols= !Orthogroup, names_to = 'strain', values_to = 'id') %>%
  mutate(files=
           case_when(
             strain == 'Bacillus_altitudinis_23_x' ~ 'alt',
             strain == 'Bacillus_atrophaeus_90_x' ~ 'atr',
             strain == 'Bacillus_infantis_161_d' ~ 'inf',
             strain == 'Bacillus_thuringiensis_111_x' ~ 'thur',
             strain == 'Corynebacterium_sp_29_x' ~ 'cor',
             strain == 'Metabacillus_indicus_450_x' ~ 'ind',
             strain == 'Micrococcus_luteus_149_a' ~ 'lut',
             strain == 'Priestia_megaterium_447_x' ~ 'mega',
             strain == 'Staphylococcus_arlettae_99_b' ~ 'arl',
             strain == 'Staphylococcus_shinii_154_a' ~ 'shin'
           ))





#+ reads in fasta fromat and names() reduces the complete names to just the 
#+ first part which is the one in the s$id df

alt <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Bacillus_altitudinis_23_x.ffn")
names(alt) <- sub(" .*", "", names(alt))

atr <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Bacillus_atrophaeus_90_x.ffn")
names(atr) <- sub(" .*", "", names(atr))

inf <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Bacillus_infantis_161_d.ffn")
names(inf) <- sub(" .*", "", names(inf))

thur <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Bacillus_thuringiensis_111_x.ffn")
names(thur) <- sub(" .*", "", names(thur))

cor <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Corynebacterium_sp_29_x.ffn")
names(cor) <- sub(" .*", "", names(cor))

ind <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Metabacillus_indicus_450_x.ffn")
names(ind) <- sub(" .*", "", names(ind))

lut <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Micrococcus_luteus_149_a.ffn")
names(lut) <- sub(" .*", "", names(lut))

mega <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Priestia_megaterium_447_x.ffn")
names(mega) <- sub(" .*", "", names(mega))

arl <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Staphylococcus_arlettae_99_b.ffn")
names(arl) <- sub(" .*", "", names(arl))

shin <- readDNAStringSet("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Staphylococcus_shinii_154_a.ffn")
names(shin) <- sub(" .*", "", names(shin))


#+ a for loop that, first separates the orthogroups, and for each orthogroup 
#+ it goes to each row and selects the if and the file, and with the get fxn 
#+ it gets the id from the file and stores it in seqs, at the end of the 
#+ embedded for loop the seqs variablewill have all the sequences of the ids 
#+ in that orthogroup and it exists the loop and writes it into fasta.
#+ And that is repeated for all orthogroups


dir.create("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Orthogroup_FASTAs", showWarnings = FALSE)


for (og in unique(s$Orthogroup)) {
  o <- s %>%
    filter(Orthogroup == og)
  
  seqs <- DNAStringSet()
  
  for (i in seq_len(nrow(o))) {
    
    gene_id <- o$id[i]
    genome <- o$strain[i]
    file <- o$files[i]
    
    seq <- get(file)[gene_id]
    seqs <- c(seqs, seq)
    
  }
  writeXStringSet(
    seqs,
    width=100000, 
    filepath = file.path("C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/analisis/genome_annot/Orthogroup_FASTAs", 
                         paste0(og, ".fasta")))
}




