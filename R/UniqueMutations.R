UniqueMutations <- 
function(hseqs){ 
    if(!is(hseqs,"DNAStringSet") & !is(hseqs,"AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet \n")
    mut.tbl <- MutsTbl(hseqs)
    return(sum(apply(mut.tbl,1,function(x) sum(x>0))))
}
