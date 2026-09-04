#toma abi files 27F y 1492R de cada cepa y las mergea tomando en cuenta q score
#objetivo: hacer un fasta file con todas las cepas para usar como referencia en vsearch qiime 
#descargar abi files de dropbox a carpeta para ABIF_Directory

#installation
#https://www.bioconductor.org/packages/release/bioc/html/sangeranalyseR.html
install.packages("BiocManager")
BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))
BiocManager::install("sangeranalyseR")

library ("sangeranalyseR")

dir.create("~/LIIGH/data/data_comsint_4c/sangercontig_fasta_files_2/")
dir.create("~/LIIGH/data/data_comsint_4c/sangercontig_fasta_files/report_abi_sangercontig_alignments_2/")



REGEX_SuffixForward <- "_27F_NS_Premix.ab1" 
REGEX_SuffixReverse <- "_1492R_NS_Premix.ab1"
contigname <- c('CH149a', "CH447")

#'NS_CH23', 'NS_CH25', 'NS_CH29', 'NS_CH99', 'NS_CH109', 'NS_CH111','NS_CH149', 'NS_CH161', 'NS_CH448', 'NS_CH450', 'NS_CH447'

for (name in contigname){
  sanger_contig<-SangerContig(
    ABIF_Directory = "C:/Users/natal/Documents/LIIGH/data/data_comsint_4c/HC0UNIDOS", #descargar abi files de dropbox a carpeta para ABIF_Directory
    inputSource="ABIF",
    contigName = name,
    REGEX_SuffixForward = REGEX_SuffixForward, 
    REGEX_SuffixReverse = REGEX_SuffixReverse
  )
  
  generateReportSC(sanger_contig, 
                   outputDir = "~/LIIGH/data/data_comsint_4c/sangercontig_fasta_files_2/report_abi_sangercontig_alignments_2/")
  
  #three fasta files: contig(27F y 1429R unidos), alignment y unalignment
  fasta<-writeFastaSC(sanger_contig, outputDir = "~/LIIGH/data/data_comsint_4c/sangercontig_fasta_files_2/")
  
}

