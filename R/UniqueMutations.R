UniqueMutations <- 
function(hseqs){ 
    if(class(hseqs)!="DNAStringSet" & class(hseqs)!="AAStringSet") 
        stop("The input object must be DNAStringSet or AAStringSet \n")
    mut.tbl <- MutsTbl(hseqs)
    return(sum(apply(mut.tbl,1,function(x) sum(x>0))))
}
