Diverge <-
function(vm,seq){ 
    if(!is(seq,"character"))
        seq <- as.character(seq)
    if( !all(strsplit(seq,"")[[1]] %in% DNA_BASES)) 
        stop("The seq argument must be a DNA sequence")
    if(!is(vm,"numeric") & !is(vm,"integer") )
        stop("The vm argument must be numeric")
    mutate <- function(nt){ 
        nt.nms <- DNA_BASES
        pnt <- rep(1/3,4)
        names(pnt) <- nt.nms
        fnt <- pnt; fnt[nt] <- 0
        return(sample(nt.nms,size=1,prob=fnt))
    }  
    ntv <- strsplit(seq,split="")[[1]]
    len <- length(ntv)
    nm <- length(vm)
    ipos <- sample(len,size=max(vm),replace=FALSE)
    nt.var <- sapply(ntv[ipos],mutate)
    dseq <- character(nm)
    for(i in 1:nm){ 
        mseq <- ntv
        mseq[ipos[1:vm[i]]] <- nt.var[1:vm[i]]
        dseq[i] <- paste(mseq,collapse="")
    }
    return(dseq)
}
