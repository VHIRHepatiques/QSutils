GetQSData <-
function(flnm, min.pct=0.1, type="DNA"){ 
    ##  Read amplicon aligned sequences with abundance data, filter
    ##    at a minimum given abundance, and sort by mutations and
    ##    abundance.
        ##    flnm: Path to the fasta file
        ## min.pct: Minimum abundance allowed
        ##    type: DNA or AA
    if(!is(min.pct, "numeric")) 
        stop("The min.pct argument must be numeric \n")
    # Read the fasta file
    lst <- ReadAmplSeqs(flnm, type)
    # Vector for filtering
    fl <- lst$nr/sum(lst$nr)*100 >= min.pct
    # Order the fasta files filtered
    lst <- SortByMutations(lst$hseqs[fl], lst$nr[fl])
    return(list(seqs=lst$bseqs, nr=lst$nr, nm=lst$nm))
}
