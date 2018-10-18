.CorrPos <- 
    function(v, nt){ 
    ## Helper function that changes a gap or NA 
    ## for the correct nt.
    # Positions in the column with '-' or 'N'
    fl <- v %in% c("-","N")
    # Replace these positions by nt
    v[fl] <- nt
    # Return corrected column
    return (v)
    }

CorrectGapsAndNs <-
function(hseqs, ref.seq){ 
    ## Function to remove missing information in the
    ## alignment. 
            ##   hseqs: DNAStringSet with haplotypes
            ## ref.seq: Reference sequence
    if(!is(hseqs, "DNAStringSet") & !is(hseqs, "AAStringSet")){
        stop("The input object hseqs must be DNAStringSet or AAStringSet\n")}
    if(!is(ref.seq, "character") & !is(ref.seq, "DNAString") & 
        !is(ref.seq, "AAString")) 
        stop("The input object ref.seq must be of class character \n")
    # split DNAString ref.seq in bases
    rf <- strsplit(as.character(ref.seq), split ="" )[[1]]
    # split haplotypes DNAStrinSet in bases , as a matrix
    ntm <- as.matrix(hseqs)
    # repair gaps and Ns in each alignment position
    ntm <- vapply(seq_len(length(rf)), function (j) .CorrPos(ntm[,j], rf[j]),
            character(nrow(ntm)))
    # return repaired haplotypes
    if(is(hseqs, "DNAStringSet"))
        correctseq <- Biostrings::DNAStringSet(apply(ntm, 1, paste, 
                        collapse=""))
    if(is(hseqs, "AAStringSet"))
        correctseq <- Biostrings::AAStringSet(apply(ntm, 1, paste, 
                        collapse=""))
    return (correctseq)
}