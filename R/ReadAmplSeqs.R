ReadAmplSeqs <-
function(flnm, type="DNA"){ 
    ##  Read amplicon aligned sequences 
        ## flnm: path to the fasta file that contains the reads.
        ## type: DNA for nucletide sequences or 
        ##       AA for amino acid sequences.
    if(type!="AA" & type!= "DNA") stop("Check the type input \n")
    # Load the reads as AAStringSet or DNAStringSet:
    if (type == "AA") seqs <- readAAStringSet(flnm)
    if (type == "DNA") seqs <- readDNAStringSet(flnm)
    # Store in a different object the reported abundances.
    nr <- vapply(names(seqs), function(str) strsplit(str, split="\\|")[[1]][2],
                character(1))
    nr <- as.numeric(nr)
    nr[is.na(nr)] <- 1
    # Return a list with two elements: the alignment and the frequency
    return(list(nr=nr, hseqs=seqs))
}
