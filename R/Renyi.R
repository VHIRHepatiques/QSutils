Renyi <-
function(w,q)
{ if(class(w)!="numeric" & length(w)<=0) 
  stop("The input object must be a numeric vector \n")
  if(any(q<0)) stop("Renyi numbers must be positive values")
  if(length(q)>1)
  { warning("Just the frisr q value is considered\n")
    q <- q[1]
  }
  if(q==0) return(log(length(w)))
  if(q==1) return(Shannon(w))
  p <- w/sum(w)
  if(q==Inf) return( -log(max(p)) )
  return(log(sum(p^q))/(1-q))
}
