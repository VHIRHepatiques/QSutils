GiniSimpson <-
function(w){ 
    ##  Gini-Simpson unbiased estimator 
        ##    w:  vector of observed counts
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    n <- sum(w)
    if(n<2) return(NULL)
    p <- w/n
    return((1 - sum(p^2))*n/(n-1))
}
#--------------------------------------------------------------
GiniSimpsonMVUE <-
function(w){ 
    ##  MVUE Gini-Simpson 
        ##   w:  vector of observed counts or frequencies
    if(!is(w,"numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    p <- w/sum(w)
    pm1 <- (w-1)/(sum(w)-1)
    return(1 - sum(p*pm1))
}
#-------------------------------------------------------------
GiniSimpsonVar <-
function(w){
    ##  Gini-Simpson asymptotic variance (Nayak 1983,1985) 
        ##    w:  vector of observed counts
    if(!is(w,"numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    n <- length(w)
    p <- w/sum(w)
    return(4/n*(sum(p^3)-sum(p^2)^2))
}