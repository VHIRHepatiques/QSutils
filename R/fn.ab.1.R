fn.ab.1 <-
function(n,h=10000,r=0.5){ 
    if(class(n)!="numeric" & class(h)!="numeric" & class(r)!="numeric"){
        stop("All arguments must be numeric")}
    a <- floor(h*r^((1:n)-1))
    a[a<1] <- 1
    return(a)
}
