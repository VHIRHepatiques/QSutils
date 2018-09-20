SegSites <-
function(seqs){ 
    if(!is(seqs,"DNAStringSet") & !is(seqs,"AAStringSet"))
        stop("The input object must be a DNAStringSet or AAStringSet\n")
    return(sum( apply(FreqMat(seqs),2,function(x) sum(x>0)) > 1 ))
}
