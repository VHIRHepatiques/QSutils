Recollapse <-
function(seqs,nr){
    ## Reollapse haplotipes after some transformation
        ## seqs: a DNAStringSet with rawreads
        ##   nr: vector of counts
    # Read and sort the sequences.
    seqso<-seqs
    sqtbl <- sort(tapply(nr,as.character(seqso),sum),decreasing=TRUE)
    seqso <- names(sqtbl)
    names(seqso) <- seq_len(length(seqso))
    # Store the abundnaces
    nr <- as.integer(sqtbl)
    # Create an DNAStringSet or AAStringSet
    if(is(seqs,"DNAStringSet")) seqso <- DNAStringSet(seqso)
    if(is(seqs,"AAStringSet")) seqso <- AAStringSet(seqso)
    # Return a list of two elements with the alignment 
    # and the abundances. 
    return(list(nr=nr,seqs=seqso))
}