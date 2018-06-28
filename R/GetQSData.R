GetQSData <-
function(flnm,min.pct=0.1,type="DNA"){ 
    if(class(min.pct)!="numeric") stop("The min.pct argument must be numeric")
    lst <- ReadAmplSeqs(flnm,type)
    fl <- lst$nr/sum(lst$nr)*100 >= min.pct
    lst <- SortByMutations(lst$hseqs[fl],lst$nr[fl])
    return(list(seqs=lst$bseqs,nr=lst$nr,nm=lst$nm))
}
