Shannon <-
function(w){ 
    ##  Shannon entropy 
        ##  w:  vector of observed counts or frequencies
    if(!is(w,"numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    h <- length(w)
    if(h < 2) return(0)
    p <- w/sum(w)
    lgp <- ifelse(w == 0, 0, log(p))
    S <- -sum(p*lgp)
    if(all(w >= 1)) S <- S+(h-1)/(2*sum(w))
    if(S > log(h)) S <- log(h)  
    return(S)
}
#-------------------------------------------------------------
ShannonVar <-
function(w) { 
    ##  Shannon entropy asymptotic variance given a vector of counts
        ##  w:  vector of observed counts or frequencies
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    h <- length(w) 
    if(h < 2) return(0)
    N <- sum(w)
    if(N < 2) return(NULL)
    w <- w/N
    lgw <- ifelse(w == 0, 0, log(w))
    S <- -sum(w*lgw)
    return(((sum(w*lgw^2)-S^2) + (h-1)/(2*N)) / N)
}
#------------------------------------------------------------
NormShannon <-
function(w) { 
    ##  Normalized Shannon entropy given a vector of counts
        ##  w:  vector of observed counts or frequencies
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    h <- length(w)
    if(h < 2) return(0)
    S <- Shannon(w)
    return(S/log(h))
}
#-----------------------------------------------------------
NormShannonVar <-
function(w) { 
    ##  Normalized Shannon entropy asymptotic variance
        ##  w:  vector of observed counts or frequencies
    if(!is(w, "numeric") & length(w) <= 0) 
        stop("The input object must be a numeric vector \n")
    h <- length(w) 
    if(h < 2) return(0)
    N <- sum(w)
    if(N < 2) return(NULL)
    w <- w/N
    lgw <- ifelse(w == 0, 0, log(w))
    S <- -sum(w*lgw)
    return((sum(w*lgw^2)-S^2+(h-1)/(2*N)) / (log(h)^2*N))
}