GetInfProfile <-
function(seqs,nr=NULL){ 
    if(is.null(nr)) nr <-rep(1,length(seqs))
    if(length(seqs)!=length(nr)) 
        stop("The input objects must have the same length \n")
    ct <- 2
    if(class(seqs)=="AAStringSet") ct <- log2(20)
    InfContent <- function(v){ 
        v <- v/sum(v) 
        lgv <- ifelse(v==0,0,log2(v))
        return(ct+sum(v*lgv))
    }
    fm <- FreqMat(seqs,nr)
    return(apply(fm,2,InfContent))
}
