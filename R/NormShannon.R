NormShannon <-
function(w) { 
    if(!is(w,"numeric") & length(w)<=0) 
        stop("The input object must be a numeric vector \n")
    h <- length(w)
    if(h<2) return(0)
    S <- Shannon(w)
    return(S/log(h))
}
