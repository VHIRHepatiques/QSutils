GenerateVars <-
function(seq,nhpl,max.muts,p.muts){ 
    if(class(seq)!="character")
    seq <- as.character(seq)
    if( !all(strsplit(seq,"")[[1]] %in% DNA_BASES)) 
        stop("The seq argument must be a DNA sequence")
    if(class(nhpl)!="numeric") 
        stop("The nhpl argument must be numeric")
    if(length(p.muts)!=max.muts) 
        stop("The p.muts argument must have the same length as max.muts")
    mutate <- function(nt){ 
        nt.nms <- DNA_BASES
        pnt <- rep(1/3,4)
        names(pnt) <- nt.nms
        fnt <- pnt; fnt[nt] <- 0
        return(sample(nt.nms,size=1,prob =fnt))
    }  
    p.muts <- p.muts/sum(p.muts)
    ntv <- strsplit(seq,split="")[[1]]
    n.muts <- sample(max.muts,size=nhpl,prob=p.muts,replace=TRUE)
    len <-  length(ntv)
    vseqs <- character(nhpl)
    for(i in 1:nhpl){ 
        ipos <- sample(len,n.muts[i],replace=FALSE)  
        nt.var <- sapply(ntv[ipos],mutate)
        mseq <- ntv
        mseq[ipos] <- nt.var
        vseqs[i] <- paste(mseq,collapse="")
    }
    return(vseqs)
}
