RaoVar <-
function(dst,w=NULL){ 
    if(!is(dst,"dist")) stop("The input object must be dist class  \n")
    if (is.null(w)) w<- rep(1,ncol(dst))
    if(nrow(as.matrix(dst))!=length(w)) 
        stop ("w and dst must have the same length")
    n <- sum(w)
    if(n<2) return(0)
    p <- w/n
    D <- as.matrix(dst)
    S <- -(p%*%t(p))
    diag(S) <- p*(1-p)
    return(4*t(p)%*%D%*%S%*%D%*%p/n)
}
