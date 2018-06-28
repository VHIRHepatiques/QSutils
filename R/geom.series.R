geom.series <-
function(n,p=0.001){ 
    if(class(n)!="numeric" & class(p)!="numeric"){
        stop("All arguments must be numeric")}
    k <- 1:n
    return((1-p)^(k-1)*p)
}