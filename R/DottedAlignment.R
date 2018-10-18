DottedAlignment <-
function(hseqs){ 
    ## Takes first haplotype as reference and uses 
    ##    dots in conserved sites
        ## hseqs : Aligment of haplotypes. 
    if(is(hseqs, "character"))
        hseqs <- DNAStringSet(hseqs)
    if(!is(hseqs, "DNAStringSet") & !is(hseqs, "AAStringSet")) 
        stop("The input object must be DNAStringSet or AAStringSet \n")
    # Convert the alignment into matrix.
    bpm <- as.matrix(hseqs)  
    # Set the master sequence
    master <- bpm[1,]
    # Change the conserved positions for a "."
    bpm.dot <- t(apply(bpm[-1,], 1, function(x) { x[x == master] <- "."; x }))
    bpm.dot <- rbind(master, bpm.dot)
    # Create strings of each sequence. 
    seqs.dot <- apply(bpm.dot, 1, paste,collapse="")
    names(seqs.dot) <- names(hseqs)
    return(seqs.dot)
}
