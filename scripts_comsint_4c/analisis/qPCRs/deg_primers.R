#+ script for doing all the possible combinations for the degerated primers for 
#+ the TU elongation factor 

#+ degeneraed primers from consensus between the 10 strains for TU elong factor 
#+ selected by doing assembly of gene from the 10 strains on benchling .
#+ 
#+ Primers with degenerated bases 
#+ W (AT), R (AG), Y(CT), H(ACT)
#+ Primers are 5' to 3'

f_primer<-"TTCWCHATCACHGGYCGTGG"
r_primer<-"CGGAARTAGAAYTGHGGRCGGTA"

#+ Easy simple and fast solution. expand grid makes all the possible combinations 
#+ from the given options 

f <- expand.grid('T','T','C', c('A','T'),'C',c("A", "C", "T"),'A','T','C','A','C',c("A", "C", "T"),'G','G',c("C", "T"),'C','G','T','G','G')
r <- expand.grid('C','G','G','A','A',c("A", "G"),'T','A','G','A','A',c("C", "T"),'T','G',c("A", "C", "T"),'G','G',c("A", "G"),'C','G','G','T','A')

apply(f, 1, paste0, collapse = "")
apply(r, 1, paste0, collapse = "")




A4
t5
c6
g5
