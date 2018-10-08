Collapse <-
function(seqs){
    cls <- class(seqs)
    sqtbl <- sort(table(as.character(seqs)),decreasing=TRUE)
    seqs <- names(sqtbl)
    names(seqs) <- seq_len(length(seqs))
    nr <- as.integer(sqtbl)
    if(cls=="DNAStringSet") seqs <- DNAStringSet(seqs)
    if(cls=="AAStringSet") seqs <- AAStringSet(seqs)
    return(list(nr=nr,hseqs=seqs))
}