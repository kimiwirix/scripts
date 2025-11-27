# compare ST... frequency matrix of only inicial cultures (NS00001A-NS00006A, NS00049A-NS00054A) to unmatched sequences in the vsearch open cluster 
#install.packages("readODS")
#install.packages("Biostrings")
library(readODS)
library(Biostrings)


#matched: tabla de freq que macheo con cepas ST. 
#unmatched: tabla de freq con ids no macheados a cepas ST
#NS1: NS00001A-NS00006A
#NS2: NS00049A-NS00054A
#NS3: NS00091A-NS00096A
#NS4: NS00133A-NS00138A

#Parte1------------------------------------------------------------------------------------------------------------------------------

NS1<-read_ods("C:/Users/natal/Documents/LIIGH/data_NS1.ods", sheet = "syncoms")
NS2<-read_ods("C:/Users/natal/Documents/LIIGH/data_NS2.ods", sheet = "syncoms")
NS3<-read_ods("C:/Users/natal/Documents/LIIGH/data_NS3.ods", sheet = "syncoms")
NS4<-read_ods("C:/Users/natal/Documents/LIIGH/data_NS4.ods", sheet = "syncoms")
data_matched <- read_ods("C:/Users/natal/Documents/LIIGH/16S analysis/feature-table-open-all.ods", sheet = "matched")
data_unmatched <- read_ods("C:/Users/natal/Documents/LIIGH/16S analysis/feature-table-open-all.ods", sheet = "unmatched")

NS1<-as.matrix(NS1); NS1[is.na(NS1)] <- 0 #convierte a matrix y NAs a 0s
NS2<-as.matrix(NS2); NS2[is.na(NS2)] <- 0 
NS3<-as.matrix(NS3); NS3[is.na(NS3)] <- 0 
NS4<-as.matrix(NS4); NS4[is.na(NS4)] <- 0 
matched<- as.matrix(data_matched)
unmatched<- as.matrix(data_unmatched)



rownames(matched)<-matched[,1];matched<- matched[,-1] #asigna ST id a colname y elimina primera columna con ST ids 
rownames(unmatched)<-unmatched[,1];unmatched<- unmatched[,-1] 
rownames(NS1)<-NS1[,1];NS1<- NS1[,-1]
rownames(NS2)<-NS2[,1];NS2<- NS2[,-1]
rownames(NS3)<-NS3[,1];NS3<- NS3[,-1]
rownames(NS4)<-NS4[,1];NS4<- NS4[,-1]
colnames(NS1)<-c(1:6); colnames(NS2)<-c(7:12); colnames(NS3)<-c(13:18); colnames(NS4)<-c(19:24)



NS1 <- NS1[rownames(NS1) != "ST00110", , drop = FALSE] #Elimina fila de ST00110 porque no está bien la referencia
NS2 <- NS2[rownames(NS2) != "ST00110", , drop = FALSE] 
NS3 <- NS3[rownames(NS3) != "ST00110", , drop = FALSE] 
NS4 <- NS4[rownames(NS4) != "ST00110", , drop = FALSE] 

communities_day0<-c("N500001A","N500002A","N500003A","N500004A","N500005A","N500006A",
                    "N500049A","N500050A","N500051A","N500052A","N500053A","N500054A",
                    "NS00091A","NS00092A","NS00093A","NS00094A","NS00095A","NS00096A",
                    "NS00133A","NS00134A","NS00135A","NS00136A","NS00137A","NS00138A")


matched_day0<-matched[,communities_day0] #reduce columnas a solo las starting sincoms en dia 0
unmatched_day0<-unmatched[,communities_day0]

class(unmatched_day0)<-"numeric"
class(matched_day0)<-"numeric"
class(NS1)<-"numeric" ;class(NS2)<-"numeric"; class(NS3)<-"numeric"; class(NS4)<-"numeric"

NS_merged<-cbind(NS1,NS2,NS3,NS4)

unmatched_day0_reduced<-unmatched_day0[rowSums(unmatched_day0[])>100,] #quita las filas que no sumen mas de 100 
unmatched_day0_reduced<-ifelse(unmatched_day0_reduced>0, 1, 0) #convierte los numeros mayores a 0 en 1 


#Parte2-------------------------------------------------------------------------------------------------------------------------------------------

#compara los datos de presencia y ausencia de las syncoms de NS y las compara con la tabla para ver de las unmatched cual pertence a cual 
#asigna cada strain ST a bact strain y compara los patrones que deberían tener con os que hay en la tabla de freq unmatched y appendea el ST que salio matched
x<-1
for (x in 1:9) {
  bact_strain<-NS_merged[x,]
  
  comparison <- unmatched_day0_reduced[apply(unmatched_day0_reduced, 1, function(row) all(row == bact_strain)), , drop = FALSE]#compara la linea de ST de NS comsints con las de unmatched 
  row_names_vector <- rownames(comparison) #extrae los ids de las unmatched 
  
  real_values <- unmatched_day0[rownames(unmatched_day0) %in% row_names_vector, , drop = FALSE] #con los ids de las unmatched se va a tabla completa y saca los verdaderos valores de freq
  
  row_matched<-matched_day0[rownames(NS1)[x], , drop = FALSE]# extrae la fila que si macheo de la misma cepa ST
  real_values<-rbind(real_values,row_matched) #appendea la fila si macheada a la tabla de no macheos
  
  matrix_name<-rownames(NS1)[x]#asigna nombre
  #name<-paste(matrix_name,"NS1",sep = "_")#cambiar NS1,NS2,NS3,NS4
  assign(matrix_name, real_values)
  
}


#Parte3------------------------------------------------------------------


#fxn calcula manhattan distance, si los numeros entre dos vectores son iguales plt es 0 si son diferentes plt es 1 y se van sumando als diferencias 
#resultado: un txt con los ids de las cepas unmatched que se parecen a las ST 

manhattanDistance <- function(vect1, vect2){ 
  dist <- abs(vect1 - vect2)
  dist <- sum(dist)
  return(dist)
}

#hace un vector con los ids de las seqs de unmatched que tengan valor de manhattan =0
matching_unmatched<-c()
for (i in 1:nrow(NS_merged)){
  for (x in 1:nrow(unmatched_day0_reduced)){
    md<-manhattanDistance(NS_merged[i,], unmatched_day0_reduced[x,])
    if (md==0){
      matching_unmatched<-append(matching_unmatched,rownames(unmatched_day0_reduced)[x])
    }
  }
}

matching_unmatched
write(matching_unmatched, "matching_unmatched_manhattan.txt")

#Parte4----------------------------------------------------------------------------------------
#Trimmea sequences using primers. Not automated, done manually with each sequence 


sequence<-"CAGTCGAGCGACAGATAGGAGCTTGCTCCTTTGACGTTAGCGGCGGACGGGTGAGTAACACGTGGGTAACCTACCTATAAGACTGGAATAACTCCGGGAAACCGGGGCTAATGCCGGATAACATTTAGAACCGCATGGTTCTAAAGTGAAAGATGGTTTTGCTATCACTTATAGATGGACCCGCGCCGTATTAGCTAGTTGGTAAGGTAATGGCTTACCAAGGCAACGATACGTAGCCGACCTGAGAGGGTGATCGGCCACACTGGAACTGAGACACGGTCCAGACTCCTACGGGAGGCAGCAGTAGGGAATCTTCCGCAATGGGCGAAAGCCTGACGGAGCAACGCCGCGTGAGTGATGAAGGGTTTCGGCTCGTAAAACTCTGTTATTAGGGAAGAACAAACGTGTAAGTAACTGTGCACGTCTTGACGGTACCTAATCAGAAAGCCACGGCTAACTACGTGCCAGCAGCCGCGGTAATACGTAGGTGGCAAGCGTTATCCGGAATTATTGGGCGTAAAGCGCGCGTAGGCGGTTTCTTAAGTCTGATGTGAAAGCCCACGGCTCAACCGTGGAGGGTCATTGGAAACTGGGAGACTTGAGTGCAGAAGAGGAAAGTGGAATTCCATGTGTAGCGGTGAAATGCGCAGAGATATGGAGGAACACCAGTGGCGAAGGCGACTTTCTGGTCTGTAACTGACGCTGATGTGCGAAAGCGTGGGGATCAAACAGGATTAGATACCCTGGTAGTCCACGCCGTAAACGATGAGTGCTAAGTGTTAGGGGGTTTCCGCCCCTTAGTGCTGCAGCTAACGCATTAAGCACTCCGCCTGGGGAGTACGACCGCAAGGTTGAAACTCAAAGGAATTGACGGGGACCCGCACAAGCGGTGGAGCATGTGGTTTAATTCGAAGCAACGCGAAGAACCTTACCAAATCTTGACATCCTTTGACCACTCTAGAGATAGAGTTTTCCCCTTCGGGGGACAAAGTGACAGGTGGTGCATGGTTGTCGTCAGCTCGTGTCGTGAGATGTTGGGTTAAGTCCCGCAACGAGCGCAACCCTTAAACTTAGTTGCCAGCATTTAGTTGGGCACTCTAGGTTGACTGCCGGTGACAAACCGGAGGAAGGTGGGGATGACGTCAAATCATCATGCCCCTTATGATTTGGGCTACACACGTGCTACAATGGACAATACAAAGGGCAGCTAAACCGCGAGGTCATGCAAATCCCATAAAGTTGTTCTCAGTTCGGATTGTAGTCTGCAACTCGACTACATGAAGCTGGAATCGCTAGTAATCGTAGATCAGCATGCTACGGTGAATACGTTCCCGGGTCTTGTACACACCGCCCGTCACACCACGAGAGTTTGTAACACCCGAAGCCGGTGGAGTAACCATTTATGGAGCTAGCC"
primer_forward<- "CCTACGGGAGGCAGCAG" #515R primer that targets V4 region
primer_reverse<- "ATTAGATACCCTGGTAGTCC" #806r primer that targets v4 region GGACTACNVGGGTWTCTAAT

#position of the forward and reverse primers in the sequence
fwd_pos <- matchPattern(primer_forward, sequence)  
rev_pos <- matchPattern(primer_reverse, sequence)  

#extracts the portion of the sequence with primers included
#trimmed_sequence <- subseq(sequence, start=start(fwd_pos), end=end(rev_pos))

#extracts the portion of the sequence without primers 
trimmed_sequence <- subseq(sequence, start=end(fwd_pos+1), end=start(rev_pos+1))
trimmed_sequence




#Parte 5------------------------------------------------------------------
#suma de las tablas individuales por cepa de parte 3 para hacer una tabla 

#cargar tabla cpn matched y unmatched, no se carga arriba porque es muy pesada y solo se usa para esta parte
data_all <- read_ods("C:/Users/natal/Documents/LIIGH/16S analysis/feature-table-open-all.ods", sheet = "feature-table-open-all")
all<- as.matrix(data_all)
rownames(all)<-all[,1];all<- all[,-1] 
class(all)<-"numeric"



total_abundance_table<-matrix(ncol = 120, nrow=0)#open new matrix. Set ncol to all columns of all days 
for (x in 1:9){ 
  #Access the ST ids and get the matrixes made in part3 for getting the unmatched and the matched together
  strain<-rownames(NS1)[x]
  individual_matrix_reduced<-get(strain)
  
  individual_matrix <- all[rownames(individual_matrix_reduced), , drop = FALSE] #extract the rows from matrix all with the rwonames from matrix individual_reduced from part 3
  
  summed_matrix<-matrix(colSums(individual_matrix),nrow=1) #sums cols and returns matrix with one row 
  
  rownames(summed_matrix)<-strain
  colnames(summed_matrix)<-colnames(individual_matrix)
  
  
  total_abundance_table<-rbind(total_abundance_table, summed_matrix)#appends results to new matrix created above
}

#hace un tsv, el cbind es para que las rownames se pasen a columna
write.table(cbind(RowName = rownames(total_abundance_table), total_abundance_table), file = "C:/Users/natal/Documents/LIIGH/16S analysis/total_abundance_table.tsv", sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


total_abundance_table
















