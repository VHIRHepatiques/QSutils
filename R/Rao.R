Rao <-
function(dst, w=NULL){
    ## Rao’s functional entropy
        ##   dst: a distance object or an aquare matrix of distances
        ##     w: vector of counts  
    if(!is(dst, "dist") & !is(dst, "matrix"))
    stop("The input object must be of dist or matrix class \n")
    if (is.null(w)) w<- rep(1, ncol(dst))
    D <- as.matrix(dst)
    if(nrow(D) != length(w)) stop ("w and dst must have the same dimension \n")
    n <- sum(w)
    if(n < 2) return(0)
    p <- w/n
    return((n/(n-1))*(t(p) %*% D %*% p))
}
#--------------------------------------------------------------------
RaoPow <-
function(dst,q,w=NULL){ 
    ## Rao entropy of order q,
        ##   dst: a distance object or an aquare matrix of distances
        ##     q: exponent
        ##     w: vector of counts  
    if(is(dst, "matrix")){ dst<-as.dist(dst)}
    if(!is(dst, "dist")) 
        stop("The input object must be of dist or matrix class  \n")
    D <- as.matrix(dst)
    if (is.null(w)) w<- rep(1, attr(dst, "Size"))
    if (attr(dst, "Size") != length(w)) 
        stop ("w and dst must have the same dimension")
    if(!is(q, "numeric")) stop("The q input object must be numeric \n")
    if(length(q) > 1){ 
        warning("Just the first q value is considered \n")
        q <- q[1]
    } 
    n <- sum(w)
    if(n<2) return(NULL)
    p <- w/n
    O <- p %*% t(p)   #  |p><p|
    res <- sum(D*O^q)
    return(res)
}
#--------------------------------------------------------------------
RaoPowProfile <-
function(dst, w=NULL, q=NULL){
    ## Computes a profile of RaoPow entropy with increasing q.
        ##   dst: a distance object or an aquare matrix of distances
        ##     q: exponent
        ##     w: vector of counts  
    if(!is(dst, "dist") & !is(dst, "matrix"))
    stop("The input object must be of dist or matrix class  \n")
    if (is.null(w)) w<- rep(1, ncol(dst))
    if(nrow(as.matrix(dst)) != length(w))
        stop ("w and dst must have the same dimension \n")
    if(is.null(q))
        q <- seq(0, 2, 0.1)
    m <- length(q)
    dv <- vapply(seq_len(m), function(i) RaoPow(dst, q[i], w),numeric(1))
    return(data.frame(q=q, qQ=dv))
}
#--------------------------------------------------------------------
RaoVar <-
function(dst,w=NULL){ 
    ## Variance of the Rao estimator
        ##   dst: a distance object or an aquare matrix of distances
        ##     w: vector of counts
    if(!is(dst, "dist")) stop("The input object must be dist class  \n")
    if (is.null(w)) w<- rep(1, ncol(dst))
    if(nrow(as.matrix(dst)) != length(w)) 
        stop ("w and dst must have the same length \n")
    n <- sum(w)
    if(n<2) return(0)
    p <- w/n
    D <- as.matrix(dst)
    S <- -(p%*%t(p))
    diag(S) <- p*(1-p)
    return(4*t(p)%*%D%*%S%*%D%*%p/n)
}

