SegSites <-
function(seqs){ 
    ##  Segregating sites: Number of sites with mutations
        ##  seqs: an aligment of haplotypes
    if(!is(seqs, "DNAStringSet") & !is(seqs, "AAStringSet"))
        stop("The input object must be a DNAStringSet or AAStringSet\n")
    return(sum(colSums(FreqMat(seqs)> 0)> 1))
}
