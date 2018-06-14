GiniSimpsonVar <-
function(w)
{ if(class(w)!="numeric" & length(w)<=0) stop("The input object must be a numeric vector \n")
  n <- length(w)
  p <- w/sum(w)
 return(4/n*(sum(p^3)-sum(p^2)^2))
}
