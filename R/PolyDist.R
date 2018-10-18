PolyDist <-
function(seqs, w=NULL){ 
    ##  Fraction of substitutions by polymorphic site
        ## seqs: a DNAStringSet with haplotypes
        ##    w: vector of counts
    if(!is(seqs, "DNAStringSet") & !is(seqs, "AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet \n")
    if(is.null(w)) w <- rep(1, length(seqs))
    if(length(seqs) != length(w)) 
        stop("The input objects must have the same length \n")
    # Compute the matrix of frequencies
    seq.tbl <- FreqMat(seqs, w)
    # Number of sequences:
    nt <- sum(seq.tbl[,1]) 
    # Compute the table of mutations 
    seq.tbl <- MutsTbl(seqs, w)
    # Save those columns with mutations
    seq.tbl <- seq.tbl[,colSums(seq.tbl) > 0,drop=FALSE]
    return(colSums(seq.tbl)/nt)
}
