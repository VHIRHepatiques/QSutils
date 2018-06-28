TotalMutations <-
function(hseqs,w=NULL){ 
    if(class(hseqs)!="DNAStringSet" & class(hseqs)!="AAStringSet") 
        stop("The input object must be DNAStringSet or AAStringSet \n")
    if(is.null(w)) 
        w <- rep(1,length(hseqs))
    if(length(hseqs)!=length(w)) 
        stop("The input objects must have the same length \n")
    mut.tbl <- MutsTbl(hseqs,w)
    return(sum(apply(mut.tbl,1,function(x) sum(x))))
}
