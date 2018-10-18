MutsTbl <-
function(hseqs, nr=NULL){ 
    ##  Matrix of mutation frequencies by position
        ##   hseqs: a DNAStringSet with haplotypes
        ##      nr: Vector of abundances
    # Set the abundances to 1 if nr is null
    if(is.null(nr)) 
    nr <- rep(1, length(hseqs))
    # Compute the matrix of frequencies
    seq.tbl <- FreqMat(hseqs, nr)
    # Store the maximum of each column
    j <- apply(seq.tbl, 2, function(x) which.max(x)[1])
    # Set to 0 those positions with the max per column
    seq.tbl[cbind(j, seq_len(ncol(seq.tbl)))] <- 0
    return(seq.tbl)
}
