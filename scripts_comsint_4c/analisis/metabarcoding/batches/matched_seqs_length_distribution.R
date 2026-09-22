#+ ESTE SCRIPT SE CORRIÓ EN EL CLUSTER. VER: /mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE


#+ plot de la distribución de lengths de todo (dereplicated seqs), de lo que matcheo y de 
#+ lo que no matecheo a las refrencias. Para saber que matcheo y que no matco se usaron los 
#+ datos de blast vsearch (global alignment). VER: 
#+ /mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/metabarcoding_analysis_vsearch.sh

#+ Con el objetivo de usar un length para filtrar las dereplicated seqs (length sugerido 320bp)


library(tibble)
library(dplyr)
library(Biostrings)
library(ggplot2)



print("empieza")

#+ dereplicated-seqs.fasta son todas las secuencias que existen de la secuenciacion 
#+ pero se agrupan las secuencias que son idénticas . El .fasta contiene el 
#+ id: >0bf90caf3253bccdc2da98670f04fcf21cfe682f, la muestra: CC0033X_17 
#+ y la secuencia

#+ dereplicated seqs, with no length parameter y removes the CCpart of the sequence id 
#+ (ej: >a73514fb4b2c84648d2918829e0e8ddddfcfacf5 CC0507X_30334255) to match the id 
#+ from the vsearch blast & converts to df with the id (names) and the width of the sequence

dereplicated_seqs <- readDNAStringSet('/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/dereplicated-seqs/dna-sequences.fasta')
names(dereplicated_seqs) <- sub(" CC0.*$", "", names(dereplicated_seqs))


d <- data.frame(
  names = names(dereplicated_seqs),
  width = width(dereplicated_seqs))


#+ Plots the widths of the sequences 

plot1<-ggplot(d, aes(x = width)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(100,600,20))

ggsave(plot = plot1,
         filename = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE/derep_distrib_lengths.png",
         bg="white",  width = 30, height = 17, units = "cm")




print("head de dereplicated seqs:")
head(d)


#+ vsearch done on dereplicated-seqs.qza with no length treatment, with 0.99 perc ident, 
#+ and min query coverage of 0 trying to be similar to what vsearch open reference does, 
#+ since from vsearch open reference I cannot extract the matched and unmatched sequences ids
#+ to make the lengths distribution plots 

vsearch <- read.table(file = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/vsearch-global-results/blast6.tsv", header = FALSE)


print('ya cargo vsearch y tiene estas filas')
nrow(vsearch)
head(vsearch)


#+ El vsearch debería tener el mismo numero de filas que el 
#+ dereplicated sequences, pero no lo tienen porque una secuencia puede estar 
#+ matecheando al 100% con una cepa y al 95% con otra. Entonces, de los reads
#+ tengo que quitar los que estén duplicados dejando el que tiene mayor porcentaje
#+ identidad (V3). y si los %ident son los mismos entonces dejar los que que tengan 
#+ menor número de mismatches (V5)

#+ there are duplicated ids since one read can map to different reference strains so this selects
#+ removes the duplicated reads mantaining the one with highest perc ident 


v <- vsearch %>%
  group_by(V1) %>%
  slice_max(V3, n = 1, with_ties = TRUE) %>%
  slice_min(V5, n = 1, with_ties = TRUE) %>%
  ungroup()


print('ya se filtro vsearch y la tabla tiene estas filas')
nrow(v) 
head(v)


write.table(v, file = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE/vsearch_filtered.tsv", row.names=FALSE, sep="\t")


print("ya se guardo la vsearch filtered table")


#+ crea tabla de ids de unmatched que son los que en segunda columna (subject) tiene un * 
#+ y hace left join con el dereplicated sequences para añadir el length de las unmatched 

#+ Hace lo mismo con matcehd pero las columnas que no tienen el *, o sea que si matchearon a una referencia 

#+ hace lo mismo pero para todas las secuencias 

unmatch <- v %>%
  filter(V2 == '*')%>%
  select (V1) %>%
  left_join(d, by = c("V1" = "names"))

match <- v %>%
  filter(!(V2 == '*'))%>%
  select (V1) %>%
  left_join(d, by = c("V1" = "names"))


all <- v %>%
  select (V1) %>%
  left_join(d, by = c("V1" = "names"))


print("matched tabla")
nrow(match)
head(match)

print("unmatched table")
nrow(unmatch)
head(unmatch)

print("all tabla")
nrow(all)
head(all)

#+ plots the distributions and saves the plots 

plot2 <- ggplot(match, aes(x = width)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(200,500,20))

plot3<-ggplot(unmatch, aes(x = width)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(200,500,20))

plot4<-ggplot(all, aes(x = width)) +
  geom_histogram(binwidth = 5, boundary = 0) +
  scale_y_log10() +
  scale_x_continuous(breaks = seq(200,500,20))


ggsave(plot = plot2,
         filename = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE/matched_distrib_lenghts.png",
         bg="white",  width = 30, height = 17, units = "cm")
ggsave(plot = plot3,
         filename = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE/unmatched_distrib_lengths.png",
         bg="white",  width = 30, height = 17, units = "cm")
ggsave(plot = plot4,
         filename = "/mnt/data/sur/users/nsaid/4c/metabarcoding/analysis/blast/ERRE/all_distrib_lengths.png",
         bg="white",  width = 30, height = 17, units = "cm")

print("ya acabo esto")
