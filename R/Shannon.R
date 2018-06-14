Shannon <-
function(w)
{ if(class(w)!="numeric" & length(w)<=0) 
    stop("The input object must be a numeric vector \n")
  h <- length(w)
  if(h<2) return(0)
  p <- w/sum(w)
  lgp <- ifelse(w==0,0,log(p))
  S <- -sum(p*lgp)
  if(all(w>=1)) S <- S+(h-1)/(2*sum(w))
  if(S>log(h)) S <- log(h)  
  return(S)
}
