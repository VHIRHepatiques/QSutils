RaoPow <-
function(dst,q,w=NULL){ 
    if(class(dst)!="dist" & class(dst)!="matrix") 
        stop("The input object must be of dist or matrix class  \n")
    D <- as.matrix(dst)
    if (is.null(w)) w<- rep(1,attr(dst,"Size"))
    if (attr(dst,"Size")!=length(w)) 
        stop ("w and dst must have the same dimension")
    if(class(q)!="numeric") stop("The q input object must be numeric\n")
    if(length(q)>1){ 
        warning("Just the first q value is considered.\n")
        q <- q[1]
    } 
    n <- sum(w)
    if(n<2) return(NULL)
    p <- w/n
    O <- p %*% t(p)   #  |p><p|
    res <- sum(D*O^q)
    return(res)
}
