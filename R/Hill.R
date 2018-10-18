Hill <-
function(w, q){
    ##  Hill entropy
        ##   w:  vector of observed counts or frequencies
        ##   q:  exponent
    if(!is(w,"numeric") & length(w) <= 0)
        stop("The input object must be a numeric vector \n")
    if(any(q < 0)) stop("Hill numbers must be positive \n")
    if(length(q) > 1) {
        warning("Just the first q value is considered \n")
        q <- q[1]
    }
    if(q == 0) return(length(w))
    if(q == 1) return(exp(Shannon(w)))
    p <- w/sum(w)
    if(q == Inf) return(1/max(p))
    if(q == -Inf) return(1/min(p))
    return(sum(p^q)^(1/(1-q)))
}
#--------------------------------------------------------------------
HillProfile <-
function(w, q=NULL){
    ## Computes a profile of Hill numbers with increasing q.
        ##    w:  vector of observed counts or frequencies
        ##    q:  exponent
    if(!is(w, "numeric") & length(w) <= 0)
        stop("The input object must be a numeric vector \n")
    if(is.null(q))
        q <- c(seq(0, 0.9, 0.1), seq(1, 1.8, 0.2), seq(2, 3.75, 0.25),
        seq(4, 10, 1), Inf)
    dv <- vapply(q, function(e) Hill(w, e), numeric(1))
    return(data.frame(q=q, qD=dv))
}