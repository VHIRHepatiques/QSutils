geom.series <-
function(n, p=0.001){
    ## Compute the geometric sequence
        ##  n: Number of frequencies to compute 
        ##  p: Parameter of the geometric function.
    if(!is(n, "numeric") & !is(p, "numeric"))
        stop("All arguments must be numeric \n")
    k <- seq_len(n)
    return((1-p)^(k-1)*p)
}