fn.ab.1 <-
function(n,h=10000,r=0.5){ 
    if(!is(n,"numeric") & !is(h,"numeric") & !is(r,"numeric")){
        stop("All arguments must be numeric")}
    a <- floor(h*r^((seq_len(n))-1))
    a[a<1] <- 1
    return(a)
}
