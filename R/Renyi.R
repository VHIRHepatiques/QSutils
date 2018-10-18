Renyi <-
function(w, q){ 
    ##  Rényi entropy 
        ##   w:  vector of observed counts or frequencies
        ##   q:  exponent
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    if(any(q < 0)) stop("Renyi numbers must be positive values \n")
    if(length(q) > 1){ 
        warning("Just the first q value is considered\n")
        q <- q[1]
    }
    if(q == 0) return(log(length(w)))
    if(q == 1) return(Shannon(w))
    p <- w/sum(w)
    if(q == Inf) return(-log(max(p)))
    return(log(sum(p^q))/(1-q))
}
#---------------------------------------------------------------------------
RenyiProfile <-
function(w, q=NULL){ 
    ## Computes a profile of Rényi entropy with increasing q.
        ##    w:  vector of observed counts or frequencies
        ##    q:  exponent
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    if(is.null(q))
        q <- c(seq(0, 0.9, 0.1), seq(1, 1.8, 0.2), seq(2, 3.75, 0.25),
        seq(4, 10, 1), Inf)
    dv <- vapply(q, function(e) Renyi(w, e), numeric(1))
    return(data.frame(q=q, renyi=dv))
}

