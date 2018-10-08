RaoPowProfile <-
function(dst,w=NULL,q=NULL){
    if(!is(dst,"dist") & !is(dst,"matrix"))
    stop("The input object must be of dist or matrix class  \n")
    if (is.null(w)) w<- rep(1,ncol(dst))
    if(nrow(as.matrix(dst))!=length(w))
        stop ("w and dst must have the same dimension")
    if(is.null(q))
        q <- seq(0,2,0.1)
    m <- length(q)
    dv <- vapply(seq_len(m),function(i) RaoPow(dst,q[i],w),numeric(1))
    return(data.frame(q=q,qQ=dv))
}