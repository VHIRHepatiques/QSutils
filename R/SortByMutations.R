SortByMutations <-
function(bseqs, nr){ 
    ##  Sort haplotypes by differences and abundances
        ##  seqs: a DNAStringSet with haplotypes
        ##     w: a vector of haplotype counts
    if(!is(bseqs, "DNAStringSet") & !is(bseqs, "AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet\n")
    if(length(bseqs) != length(nr)) 
        stop("The input objects must have the same length \n")
    # Compute the differences between the master and the haplotypes.
    master <- bseqs[which.max(nr)]
    psa <- pairwiseAlignment(pattern=bseqs, subject=master)
    nm <- nmismatch(psa)
    tnm <- table(nm)
    # Order the sequences by the number of mutations
    o <- order(nm)
    bseqs <- bseqs[o]
    nr <- nr[o]
    nm <- nm[o]
    # Number of order inside each number of mutations
    isq <- unlist(sapply(seq_len(length(tnm)), function(i) seq_len(tnm[i])))
    #  Order by descendent frequency for each number of mutations
    for(i in as.integer(names(tnm))){ 
        idx <- which(nm == i)
        o <- order(nr[idx], decreasing=TRUE)
        bseqs[idx] <- bseqs[idx[o]]
        nr[idx] <- nr[idx[o]]
    }
    # Compute relative frequency
    frq <- round(nr/sum(nr)*100,2)
    # Set a name to each haplotype
    nms <- paste("Hpl", nm, sprintf("%04d",isq), sep="_")
    names(bseqs) <- nms
    return(list(bseqs=bseqs, nr=nr, nm=nm))
}
