Rao <-
function(dst, w=NULL){
    if(!is(dst,"dist") & !is(dst,"matrix"))
    stop("The input object must be of dist or matrix class \n")
    if (is.null(w)) w<- rep(1,ncol(dst))
    D <- as.matrix(dst)
    if(nrow(D)!=length(w)) stop ("w and dst must have the same dimension")
    n <- sum(w)
    if(n<2) return(0)
    p <- w/n
    return((n/(n-1)) * (t(p) %*% D %*% p))
}