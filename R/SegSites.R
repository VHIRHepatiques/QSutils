SegSites <-
function(seqs){ 
    if(class(seqs)!="DNAStringSet" & class(seqs)!="AAStringSet") 
        stop("The input object must be a DNAStringSet or AAStringSet\n")
    return(sum( apply(FreqMat(seqs),2,function(x) sum(x>0)) > 1 ))
}
