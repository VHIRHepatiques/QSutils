fn.ab <-
function(n,h=10000,r=0.5,fn="pcf"){ 
    ## Function to simulate haplotype abundances
        ##  n: Number of counts to compute
        ##  h: Highest abundance value 
        ##  r: A number to modify the abundance
        ## fn: Function to compute the abundances
    if(fn!="pcf" & fn!= "pf" & fn!="dpf"){
        stop("Check the fn argument options \n")}
    if(!is(n,"numeric") & !is(h,"numeric") & !is(r,"numeric")){
        stop("All arguments must be numeric \n")}
    if(fn == "pf") a <- floor(h*r^((seq_len(n))-1))
    if(fn == "pcf") a <- floor(h*1/(seq_len(n))^r)
    if(fn == "dfp") a <- floor(h^(1/seq_len(n)))
    a[a<1] <- 1
    return(a)
}
