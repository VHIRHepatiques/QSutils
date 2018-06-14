PolyDist <-
function(seqs,w=NULL)
{ if(class(seqs)!="DNAStringSet" & class(seqs)!="AAStringSet") 
    stop("The input object must be DNAStringSet or AAStringSet \n")
  if(is.null(w)) w <- rep(1,length(seqs))
  if(length(seqs)!=length(w)) 
    stop("The input objects must have the same length \n")
  seq.tbl <- FreqMat(seqs,w)
  nt <- sum(seq.tbl[,1]) 
  seq.tbl <- MutsTbl(seqs,w)
  seq.tbl <- seq.tbl[,apply(seq.tbl,2,function(x) sum(x)>0),drop=FALSE]
  return(colSums(seq.tbl)/nt)
}
