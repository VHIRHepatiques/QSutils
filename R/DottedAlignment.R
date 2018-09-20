DottedAlignment <-
function(hseqs){ 
    if(is(hseqs,"character"))
        hseqs <- DNAStringSet(hseqs)
    if(!is(hseqs,"DNAStringSet") & !is(hseqs,"AAStringSet")) 
        stop("The input object must be DNAStringSet or AAStringSet \n")
    bpm <- as.matrix(hseqs)  
    master <- bpm[1,]
    bpm.dot <- t(apply(bpm[-1,],1,function(x) { x[x==master] <- "."; x }))
    bpm.dot <- rbind(master,bpm.dot)
    seqs.dot <- apply(bpm.dot,1,paste,collapse="")
    names(seqs.dot) <- names(hseqs)
    return(seqs.dot)
}
