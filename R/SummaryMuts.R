SummaryMuts <-
function(seqs, w=NULL, off=0){ 
    ##  Matrix of summary population mutations
        ##  seqs: a DNAStringSet with haplotypes
        ##     w: a vector of haplotype counts
        ##   off: offset of first position in the alignment
    if(!is(seqs, "DNAStringSet") & !is(seqs, "AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet\n")
    if(length(seqs) < 2){
        warning("More than 1 sequence is needed")
        return(NULL)
    }
    # Create a vector of abundances if it is NULL
    if(is.null(w)) w <- rep(1, length(seqs))
    # Compute the matrix of frequencies.
    pos.tbl <- FreqMat(seqs, w)
    # Compute the table of mutations. 
    mut.tbl <- MutsTbl(seqs, w)
    # Store those positions with mutations
    flags <- colSums(mut.tbl) > 0
    pos <- which(flags)
    # Return a data.frame with the information 
    # of the mutated position
    res <- data.frame(pos=pos+off, t(pos.tbl[,flags]))
    return(res)
}
