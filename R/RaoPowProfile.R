RaoPowProfile <-
function(dst,w=NULL,q=NULL){
    if(class(dst)!="dist" & class(dst)!="matrix")
    stop("The input object must be of dist or matrix class  \n")
    if (is.null(w)) w<- rep(1,ncol(dst))
    if(nrow(as.matrix(dst))!=length(w))
        stop ("w and dst must have the same dimension")
    if(is.null(q))
        q <- seq(0,2,0.1)
    m <- length(q)
    dv <- sapply(1:m,function(i) RaoPow(dst,q[i],w))
    return(data.frame(q=q,qQ=dv))
}