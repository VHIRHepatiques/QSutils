GetRandomSeq <-
function(seq.len){ 
    if(!is(seq.len,"numeric")) stop("The input must be numeric")
    nt.nms <- DNA_BASES
    return(paste(sample(nt.nms,seq.len,replace=TRUE),collapse=""))
}
