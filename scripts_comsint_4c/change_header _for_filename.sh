#change header for filename
#used bc some headers had same name and pyani.sh was using them as sequence identifier plt there were 
#diff genomes with same names 


# Loop over all .fasta files in the current directory
for fasta_file in *.fasta; do
    # Read the current header (first line) of the .fasta file
    current_header=$(head -n 1 "$fasta_file")
    
    # Remove the .fasta extension from the filename
    filename_without_extension=$(basename "$fasta_file" .fasta)
    
    # Create a new header that includes the filename without the extension
    new_header=">${filename_without_extension}"

    # Replace the first line (header) of the file with the new header
    # We use `sed` to replace the first line and append the rest of the file unchanged
    sed -i "1s/.*/$new_header/" "$fasta_file"

done


