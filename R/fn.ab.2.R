fn.ab.2 <-
function(n,h=10000,r=3)
{ if(class(n)!="numeric" & class(h)!="numeric" & class(r)!="numeric"){
    stop("All arguments must be numeric")
  } 
  a <- floor(h*1/(1:n)^r)
  a[a<1] <- 1
  return(a)
}
