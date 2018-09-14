Recollapse <-
function(seqs,nr){
    cls <- class(seqs)
    sqtbl <- sort(tapply(nr,as.character(seqs),sum),decreasing=TRUE)
    seqs <- names(sqtbl)
    names(seqs) <- 1:length(seqs)
    nr <- as.integer(sqtbl)
    if(cls=="DNAStringSet") seqs <- DNAStringSet(seqs)
    if(cls=="AAStringSet") seqs <- AAStringSet(seqs)
    return(list(nr=nr,seqs=seqs))
}