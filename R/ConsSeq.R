ConsSeq <-
function(seqs,w=NULL)
{ if(class(seqs)!="DNAStringSet" & class(seqs)!="AAStringSet") 
    stop("The input object must be a DNAStringSet or AAStringSet \n")
  if(is.null(w)) w<-rep(1,length(seqs))
  if(length(seqs)!=length(w)) 
    stop("The input objects must have the same length \n")
  bnms <- DNA_BASES
  if(class(seqs)=="AAStringSet")
    bnms <- AA_ALPHABET
  ntm <- FreqMat(seqs,w)
  get.nt <- function(x)
  { idx <- which(x==max(x))
    if(length(idx)<2) return(idx)
    return( sample(idx,1) )
  }
  imx <- apply(ntm,2,get.nt)
  return(paste(bnms[imx],collapse=""))
}
