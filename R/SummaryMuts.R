SummaryMuts <-
function(seqs,w=NULL,off=0)
{ if(class(seqs)!="DNAStringSet" & class(seqs)!="AAStringSet") 
   stop("The input object must be DNAStringSet or AAStringSet\n")
  if(length(seqs)<2){
    warning("More than 1 sequence is needed")
    return(NULL)
  }
  if(is.null(w)) w <- rep(1,length(seqs))
  pos.tbl <- FreqMat(seqs,w)
  mut.tbl <- MutsTbl(seqs,w)
  flags <- apply(mut.tbl,2,sum)>0
  pos <- which(flags)
  res <- data.frame(pos=pos+off,t(pos.tbl[,flags]))
  return(res)
}
