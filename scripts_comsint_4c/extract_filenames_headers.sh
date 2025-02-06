#script que yo le de los nombres de un fasta y se meta al fasta y me de la primera linea y me escriba 
#un .tsv con filename y su correspondiente filename 


# Makes a file where the headers and filenames are stored
> "headers.tsv"


# Loop over all .fasta files in the current directory
for fasta_file in *.fasta; do
    # Extract the first line (header) of each .fasta file
    header=$(head -n 1 "$fasta_file")
    
    # Append the filename and header to the output file
    echo -e "$fasta_file\t$header" >> "headers.tsv"
done

#hacer lipmpieza posterior de headers en excel o notepad++ manual



