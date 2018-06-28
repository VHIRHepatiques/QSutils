MutationFreqVar <-
function(nm,nr=NULL,len=1){ 
    if(is.null(nr)) nr <- rep(1,length(nm))
    if(!length(nm)==length(nr)) 
        stop("The inputs nr and nm must have the same length \n")
    N <- sum(nr)
    if(N<2) return(0)
    p <- nr/sum(nr)
    v <- ((sum(p*nm^2)-sum(p*nm)^2)/len^2) / N
    names(v) <- NULL
    return(v)
}
