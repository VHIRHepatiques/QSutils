MutationFreq <-
function(dst=NULL,nm=NULL,nr=NULL,len=1){ 
    if(!is.null(dst)){
        if(!is(dst,"dist") & !is(dst,"matrix")) 
            stop("The input object must be dist or matrix class \n")
        nru <- rep(1,nrow(as.matrix(dst)))
        mf <- sum(as.matrix(dst)[1,]*nru)/sum(nru)
    } else {
        if(length(nm)!=length(nr)) 
            stop("The inputs nr and nm must have the same length \n")
    mf <- sum(nm*nr/sum(nr))/len
    names(mf) <- NULL
    }
    return(mf)
}
