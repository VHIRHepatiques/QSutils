GiniSimpson <-
function(w){ 
    if(!is(w,"numeric") & length(w)<=0) 
        stop("The input object must be a numeric vector \n")
    n <- sum(w)
    if(n<2) return(NULL)
    p <- w/n
    return((1 - sum(p^2))*n/(n-1))
}
