#toma abi files 27F y 1492R de cada cepa y las mergea tomando en cuenta q score
#objetivo: hacer un fasta file con todas las cepas para usar como referencia en vsearch qiime 
#descargar abi files de dropbox a carpeta para ABIF_Directory

#installation
#https://www.bioconductor.org/packages/release/bioc/html/sangeranalyseR.html
install.packages("BiocManager")
BiocManager::install(c("GenomicFeatures", "AnnotationDbi"))
BiocManager::install("sangeranalyseR")

library ("sangeranalyseR")

dir.create("~/LIIGH/16S analysis/report_abi_sangercontig_alignments/")
dir.create("~/LIIGH/16S analysis/sangercontig_fasta_files/")


REGEX_SuffixForward <- "_27F_Premix.ab1" 
REGEX_SuffixReverse <- "_1492R_Premix.ab1"
contigname<-c('NS_042', 'NS_046','NS_060', 'NS_094','NS_101', 'NS_109','NS_110', 'NS_143', 'NS_154', 'NS_164')


for (name in contigname){
  sanger_contig<-SangerContig(
    ABIF_Directory = "~/LIIGH/16S analysis/DROPBOX_abi_fasta_files_comsintstrains/", #descargar abi files de dropbox a carpeta para ABIF_Directory
    inputSource="ABIF",
    contigName = name,
    REGEX_SuffixForward = REGEX_SuffixForward, 
    REGEX_SuffixReverse = REGEX_SuffixReverse
  )
  
  generateReportSC(sanger_contig, 
                   outputDir = "~/LIIGH/16S analysis/report_abi_sangercontig_alignments/")
  
  #three fasta files: contig(27F y 1429R unidos), alignment y unalignment
  fasta<-writeFastaSC(sanger_contig, outputDir = "~/LIIGH/16S analysis/sangercontig_fasta_files/")
  
}


