UniqueMutations <- 
function(hseqs){ 
    ## Computes the number of unique mutations in the alignment
        ##  seqs: an aligment of haplotypes
    if(!is(hseqs, "DNAStringSet") & !is(hseqs, "AAStringSet"))
        stop("The input object must be DNAStringSet or AAStringSet \n")
    mut.tbl <- MutsTbl(hseqs)
    return(sum(rowSums(mut.tbl)))
}
