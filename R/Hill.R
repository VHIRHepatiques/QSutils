Hill <-
function(w,q){
    if(!is(w,"numeric") & length(w)<=0)
        stop("The input object must be a numeric vector\n")
    if(any(q<0)) stop("Hill numbers must be positive\n")
    if(length(q)>1) {
        warning("Just the first q value is considered\n")
        q <- q[1]
    }
    if(q==0) return(length(w))
    if(q==1) return( exp(Shannon(w)))
    p <- w/sum(w)
    if(q==Inf) return(1/max(p))
    if(q==-Inf) return(1/min(p))
    return(sum(p^q)^(1/(1-q)))
}