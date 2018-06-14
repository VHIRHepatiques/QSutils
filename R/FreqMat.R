FreqMat <-
function(seqs,nr=NULL)
{ if(class(seqs)!="DNAStringSet" & class(seqs)!="AAStringSet") 
    stop("The input object must be a DNAStringSet or AAStringSet \n")
  nt.nms <- DNA_BASES
  if (class(seqs)=="AAStringSet") nt.nms <- AA_STANDARD
  if (is.null(nr)) nr <- rep(1,length(seqs))
  if(length(seqs)!=length(nr)) 
    stop("The input objects must have the same length \n")
  strm <- as.matrix(seqs)
  res <-  apply(strm,2,function(x) 
    tapply(nr,factor(x,levels=nt.nms),sum))
  colnames(res) <- 1:ncol(res)
  res[is.na(res)] <- 0
  return(res)
}
