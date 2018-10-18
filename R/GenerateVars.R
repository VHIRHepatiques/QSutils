.mutate <- function(nt){ 
    ## Function to mutate 1 given nucleotide
        ##  nt: Nucletotide to mutate
    nt.nms <- DNA_BASES
    pnt <- rep(1/3, 4)
    names(pnt) <- nt.nms
    # The prob of the nt in sample is 0.
    fnt <- pnt; fnt[nt] <- 0
    return(sample(nt.nms, size=1, prob =fnt))
}  
GenerateVars <-
function(seq, nhpl, max.muts, p.muts){ 
    ##  Generate variants of a given haplotype
        ##      seq: Sequence to create the variants
        ##     nhpl: Number of haplotypes
        ## max.muts: Number maximum of mutatz
        ##   p.muts: Probaility of mutation
    if(!is(seq, "character"))
    seq <- as.character(seq)
    if( !all(strsplit(seq, "")[[1]] %in% DNA_BASES)) 
        stop("The seq argument must be a DNA sequence \n")
    if(!is(nhpl, "numeric")) 
        stop("The nhpl argument must be numeric \n")
    if(length(p.muts) != max.muts) 
        stop("The p.muts argument must have the same length as max.muts \n")
    # Probabiliy of mutation in %
    p.muts <- p.muts/sum(p.muts)
    # Split the reference sequence
    ntv <- strsplit(seq,split="")[[1]]
    # Set the number of mutations
    n.muts <- sample(max.muts, size=nhpl, prob=p.muts, replace=TRUE)
    len <-  length(ntv)
    vseqs <- character(nhpl)
    # Create new sequences with mutations
    for(i in seq_len(nhpl)){ 
        ipos <- sample(len,n.muts[i], replace=FALSE)  
        nt.var <- vapply(ntv[ipos], .mutate, character(1))
        mseq <- ntv
        mseq[ipos] <- nt.var
        vseqs[i] <- paste(mseq, collapse="")
    }
    return(vseqs)
}
