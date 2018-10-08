RenyiProfile <-
function(w,q=NULL){ 
    if(!is(w,"numeric") & length(w)<=0) 
        stop("The input object must be a numeric vector \n")
    if(is.null(q))
        q <- c(seq(0,0.9,0.1),seq(1,1.8,0.2),seq(2,3.75,0.25),
        seq(4,10,1),Inf)
    dv <- vapply(q,function(e) Renyi(w,e),numeric(1))
    return(data.frame(q=q,renyi=dv))
}
