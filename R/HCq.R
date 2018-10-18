HCq <-
function(w, q){
    ##  Havrda-Charvat estimator
        ##    w:  vector of observed counts or frequencies
        ##    q:  exponent
    if(!is(w, "numeric") & length(w) <= 0)
        stop("The input object must be a numeric vector \n")
    if(any(q < 0)) stop("HCq numbers must be positive values \n")
    if(length(q) > 1) {
        warning("Just the first q value is considered \n")
        q <- q[1]
    }
    if(q == 0) return(length(w)-1)
    if(q == 1) return(Shannon(w))
    if(q == Inf) return(0)
    p <- w/sum(w)
    return((1-sum(p^q))/(q-1))
}
#--------------------------------------------------------------------
HCqProfile <-
function(w, q=NULL){
    ## Computes a profile of increasing q.
        ##    w:  vector of observed counts or frequencies
    if(!is(w,"numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    if(is.null(q))
        q <- c(seq(0, 0.9, 0.1),seq(1, 1.8, 0.2), seq(2, 3.75, 0.25),
        seq(4, 10, 1), Inf)
    dv <- vapply(q, function(e) HCq(w, e), numeric(1))
    return(data.frame(q=q, HC=dv))
}
#--------------------------------------------------------------------
HCqVar <-
function(w, q){ 
    ##  Havrda-Charvat  asymptotic variance (Nayak 1983,1985)
        ##    w: vector of observed counts
        ##    q: exponent
    if(!is(w,"numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    if(any(q < 0)) stop("HCq numbers must be positive values \n")
    n <- sum(w)
    if(n < 2) return(NULL)
    p <- w/n
    return(1/n*(q/(q-1))^2*(sum(p^(2*q-1))-sum(p^q)^2))
}


