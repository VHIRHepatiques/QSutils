.mutate <- function(nt){ 
    ## Function to mutate 1 given nucleotide
        ##  nt: Nucletotide to mutate
    nt.nms <- DNA_BASES
    pnt <- rep(1/3,4)
    names(pnt) <- nt.nms
    # The prob of the nt in sample is 0.
    fnt <- pnt; fnt[nt] <- 0
    return(sample(nt.nms, size=1, prob=fnt))
}  

Diverge <-
function(vm,seq){ 
    ##  Generate divergent haplotypes with a common pattern
        ##   vm: Vector with number of diverging mutations 
        ##         to be generated
        ##  seq: Sequence to diverge
    if(!is(seq, "character"))
        seq <- as.character(seq)
    if( !all(strsplit(seq, "")[[1]] %in% DNA_BASES)) 
        stop("The seq argument must be a DNA sequence \n")
    if(!is(vm, "numeric") & !is(vm, "integer") )
        stop("The vm argument must be numeric \n")
    # Split the reference sequence
    ntv <- strsplit(seq, split="")[[1]]
    len <- length(ntv)
    nm <- length(vm)
    # Which positions will mutate?
    ipos <- sample(len, size=max(vm), replace=FALSE)
    # Mutate the nuceleotides in ipos
    nt.var <- vapply(ntv[ipos], .mutate, character(1))
    dseq <- character(nm)
    # Change the seq with the new mutations to diverge
    for(i in seq_len(nm)){ 
        mseq <- ntv
        mseq[ipos[seq_len(vm[i])]] <- nt.var[seq_len(vm[i])]
        dseq[i] <- paste(mseq, collapse="")
    }
    return(dseq)
}
