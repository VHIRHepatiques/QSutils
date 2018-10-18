.InfContent <- function(v, ct){ 
    ##  Information content at a site
    ##    given the nucleotides frequency
        ##  v: vector of nt counts in a single site
        ##  ct: 2 for NT and log2(20) for AA
    v <- v/sum(v) 
    lgv <- ifelse(v == 0, 0, log2(v))
    return(ct+sum(v*lgv))
}

GetInfProfile <-
function(seqs, nr=NULL){ 
    ##  Information contents profile of an alignment
        ##  seqs: a DNAStringSet with haplotypes
        ##    nr: a vector of haplotype counts
    # Set abundances to 1 if there are no input
    if(is.null(nr)) nr <-rep(1, length(seqs))
    if(length(seqs) != length(nr)) 
        stop("The input objects must have the same length \n")
    # Set the ct.
    ct <- 2
    if(is(seqs, "AAStringSet")) ct <- log2(20)
    # Compute the matrix of frequencies
    fm <- FreqMat(seqs, nr)
    # Compute the IC by position. 
    return(apply(fm, 2, .InfContent, ct))
}
