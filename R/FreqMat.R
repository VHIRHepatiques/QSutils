FreqMat <-
function(seqs, nr=NULL){ 
    ## Nucleotide frequence by position given an alignment
        ## seqs: Aligment of haplotypes. 
        ##   nr: Vector of observed haplotype counts
    if(!is(seqs, "DNAStringSet") & !is(seqs, "AAStringSet"))
        stop("The input object must be a DNAStringSet or AAStringSet \n")
    # Load DNA nucleotides
    nt.nms <- DNA_BASES
    # Load Amino Acids if seqs is AAStringSet
    if (is(seqs, "AAStringSet")) nt.nms <- AA_STANDARD
    # Set the abundances to 1, if there is no nr input
    if (is.null(nr)) nr <- rep(1, length(seqs))
    if(length(seqs) != length(nr)) 
        stop("The input objects must have the same length \n")
    # Create a matrix with the sequences.
    strm <- as.matrix(seqs)
    # Compute the frequencies of each nucleotide/ amino acid
    # by position.
    res <-  apply(strm, 2, function(x) 
        tapply(nr, factor(x, levels=nt.nms), sum))
    colnames(res) <- seq_len(ncol(res))
    res[is.na(res)] <- 0
    return(res)
}
