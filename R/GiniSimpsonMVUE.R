GiniSimpsonMVUE <-
function(w)
{ if(class(w)!="numeric" & length(w)<=0) stop("The input object must be a numeric vector \n")
  p <- w/sum(w)
  pm1 <- (w-1)/(sum(w)-1)
  return(1 - sum(p*pm1))
}
