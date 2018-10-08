SortByMutations <-
function(bseqs,nr){ 
    if(!is(bseqs,"DNAStringSet") & !is(bseqs,"AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet\n")
    if(length(bseqs)!=length(nr)) 
        stop("The input objects must have the same length \n")
    master <- bseqs[which.max(nr)]
    psa <- pairwiseAlignment(pattern=bseqs,subject=master)
    nm <- nmismatch(psa)
    tnm <- table(nm)
    o <- order(nm)
    bseqs <- bseqs[o]
    nr <- nr[o]
    nm <- nm[o]
    isq <- unlist(sapply(seq_len(length(tnm)),function(i) seq_len(tnm[i])))
    for(i in as.integer(names(tnm))){ 
        idx <- which(nm==i)
        o <- order(nr[idx],decreasing=TRUE)
        bseqs[idx] <- bseqs[idx[o]]
        nr[idx] <- nr[idx[o]]
    }
    frq <- round(nr/sum(nr)*100,2)
    nms <- paste("Hpl",nm,sprintf("%04d",isq),sep="_")
    names(bseqs) <- nms
    return(list(bseqs=bseqs,nr=nr,nm=nm))
}
