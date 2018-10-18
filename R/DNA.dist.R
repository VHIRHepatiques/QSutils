DNA.dist <-
function(seqs, model="raw", gamma=FALSE, pairwise.deletion=FALSE){ 
    ##  Matrix of DNA distance given an alignment
        ##   seqs: Aligment of haplotypes. 
        ##  model: Evolutionary model to compute genetic distance
        ##  gamma: A gamma parameter or FLASE.
        ## pairwise.deletion: A logical indicating whether to delete sites
        ## with missing data
    if(!is(seqs, "DNAStringSet"))
        stop("The input object must be DNAStringSet \n")
    # Convert the alignment into DNAbin object
    strm <- as.DNAbin(ape::as.alignment(as.matrix(seqs)), pairwise.deletion)
    # Compute the matrix of distances
    dst <- dist.dna(strm, model=model, gamma=gamma)
    # Convert NAs into 0
    dst[is.na(dst)] <- 0
    return(dst)
}
