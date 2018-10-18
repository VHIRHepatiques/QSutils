GetRandomSeq <-
function(seq.len){ 
    ##  Generate a genetic random sequence
        # seq.len: length of the new sequence
    if(!is(seq.len, "numeric")) stop("The input must be numeric \n")
    # Load nucleotide bases
    nt.nms <- DNA_BASES
    return(paste(sample(nt.nms, seq.len, replace=TRUE), 
        collapse=""))
}
