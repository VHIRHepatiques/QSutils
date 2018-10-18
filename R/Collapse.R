Collapse <-
function(seqs){
    ## Collapse reads into haplotypes and frequencies
        ## seqs: a DNAStringSet with raw reads
    # Read and sort the sequences.
    seqso<-seqs
    sqtbl <- sort(table(as.character(seqso)), decreasing=TRUE)
    seqso <- names(sqtbl)
    names(seqso) <- seq_len(length(seqso))
    # Store the abundances
    nr <- as.integer(sqtbl)
    # Create an DNAStringSet or AAStringSet
    if(is(seqs, "DNAStringSet")) seqso <- DNAStringSet(seqso)
    if(is(seqs, "AAStringSet")) seqso <- AAStringSet(seqso)
    # Return a list of two elements with the alignment 
    # and the abundances. 
    return(list(nr=nr, hseqs=seqso))
}