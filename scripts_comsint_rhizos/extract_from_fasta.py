#Script for extracting from fasta file a sequence with the sequence id and pasting it in an output file
# necesita: fasta file con todas las sequences, txt file con los ids de las sequences que se quieren extraer, nombre del output fasta file
#used for 
def fasta_file(sequences, ids, output):

    ids = open(ids,'r')
    sequences = open(sequences,'r')

    sequences=sequences.readlines()[0:]

    with open (output,"w") as new_file:
        for line in ids:
            for index,sequence in enumerate(sequences):
                if line in sequence:
                    seqs=sequences[index+1]
                    new_file.write(sequence)
                    new_file.write(seqs)

    new_file.close()

input_file_sequences= r'C:\Users\natal\Documents\LIIGH\16S analysis\unmatched-dna-sequences-all.fasta'
input_file_ids= r'C:\Users\natal\Documents\LIIGH\16S analysis\matching_unmatched_manhattan.txt'
output_file= 'matching_unmatched_manhattan_sequences.fasta'

fasta_file(input_file_sequences, input_file_ids, output_file)
