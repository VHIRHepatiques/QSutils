fn.ab.3 <-
function(n,h=10000){ 
    if(!is(n,"numeric") & !is(h,"numeric")){
        stop("All arguments must be numeric")}
    a <- floor(h^(1/1:n))
    a[a<1] <- 1
    return(a)
}
