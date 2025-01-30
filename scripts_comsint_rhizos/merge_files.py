#merge files
#used for merging .txt of reference sequnces trimmed and .fasta of manhattan matched sequences

def merge_txt_fasta(file_1,file_2,output_file):
    file1 = open(file_1, 'r')
    file2 = open(file_2, 'r')

    # Read the contents of the text files
    content1 = file1.read()
    content2 = file2.read()

    # Close the source text files
    file1.close()
    file2.close()

    # Open the destination file
    destination_file = open(output_file, 'w')

    # Write the concatenated content to the destination file
    destination_file.write(content1 + content2)
    # Close the destination file
    destination_file.close()


file_1=r"C:\Users\natal\Documents\LIIGH\16S analysis\matching_unmatched_manhattan_sequences.fasta"
file_2=r"C:\Users\natal\Documents\LIIGH\16S analysis\sangercontig_fasta_files\reference_seqs_sangercontigR_trimmed_wo_primers.txt"
output_name="merged_manhattan_references_wo_primers_for_muscle.fasta"

merge_txt_fasta(file_1,file_2,output_name)
