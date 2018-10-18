.get.nt <- function(x){ 
    ## Helper function to obtain the most abundant
    ## nucleotide. 
    idx <- which(x == max(x))
    # If there is 1 return this
    if(length(idx) < 2) return(idx)
    # If 2 are maxim return 1 randomly
    return(sample(idx, 1))
}

ConsSeq <-
function(seqs, w=NULL){
    ##  Computes the consensus sequence given alignment 
    ##  and frequencies
        #  seqs: a DNAStringSet with haplotypes
        #     w: a vector of haplotype counts
    if(!is(seqs, "DNAStringSet") & !is(seqs, "AAStringSet")) 
        stop("The input object must be a DNAStringSet or AAStringSet \n")
    if(is.null(w)) w <- rep(1, length(seqs))
    if(length(seqs) != length(w)) 
        stop("The input objects must have the same length \n")
    # Load nucleotide bases
    bnms <- DNA_BASES
    # Or load amino acids if is an AAStringSet
    if(is(seqs, "AAStringSet")) bnms <- AA_ALPHABET
    # Compute the matrix of frequency
    ntm <- FreqMat(seqs, w)
    # Obtain the most abundant nucleotide
    imx <- apply(ntm, 2, .get.nt)
    return(paste(bnms[imx], collapse=""))
}
