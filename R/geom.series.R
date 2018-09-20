geom.series <-
function(n,p=0.001){
    if(!is(n,"numeric") & !is(p,"numeric")){
        stop("All arguments must be numeric")}
    k <- 1:n
    return((1-p)^(k-1)*p)
}