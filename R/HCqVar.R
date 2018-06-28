HCqVar <-
function(w,q){ 
    if(class(w)!="numeric" & length(w)<=0) 
        stop("The input object must be a numeric vector \n")
    if(any(q<0)) stop("HCq numbers must be positive values")
    n <- sum(w)
    if(n<2) return(NULL)
    p <- w/n
    return(1/n*(q/(q-1))^2*(sum(p^(2*q-1))-sum(p^q)^2))
}
