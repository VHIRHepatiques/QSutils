TotalMutations <-
function(hseqs, w=NULL){ 
    ## Total number of mutations (Eta)
        ##  hseqs: a DNA/AAStringSet with haplotypes
        ##      w: a vector of haplotype counts
    if(!is(hseqs, "DNAStringSet") & !is(hseqs, "AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet \n")
    # Create a vector of abundances if it is NULL
    if(is.null(w)) 
        w <- rep(1, length(hseqs))
    if(length(hseqs) != length(w)) 
        stop("The input objects must have the same length \n")
    # Compute the table of mutations
    mut.tbl <- MutsTbl(hseqs,w)
    # Sum all the mutations in the table
    return(sum(rowSums(mut.tbl)))
}
