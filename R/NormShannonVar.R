NormShannonVar <-
function(w) 
{ if(class(w)!="numeric" & length(w)<=0) 
    stop("The input object must be a numeric vector \n")
  h <- length(w) 
  if(h<2) return(0)
  N <- sum(w)
  if(N<2) return(NULL)
  w <- w/N
  lgw <- ifelse(w==0,0,log(w))
  S <- -sum(w*lgw)
  return((sum(w*lgw^2)-S^2+(h-1)/(2*N)) / (log(h)^2*N))
}
