HCq <-
function(w,q)
{ if(class(w)!="numeric" & length(w)<=0) 
  stop("The input object must be a numeric vector \n")
  if(any(q<0)) stop("HCq numbers must be positive values")
  if(length(q)>1) 
  { warning("Just the first q value is considered")
    q <- q[1]
  }
  if(q==0) return(length(w)-1)
  if(q==1) return(Shannon(w))
  if(q==Inf) return(0)
  p <- w/sum(w)
  return((1-sum(p^q))/(q-1))
}