DNA.dist <-
function(seqs,model="raw",gamma=FALSE,pairwise.deletion=FALSE)
{ if(class(seqs)!="DNAStringSet") 
    stop("The input object must be DNAStringSet \n")
  strm <- as.DNAbin(ape::as.alignment(as.matrix(seqs)),pairwise.deletion)
  dst <- dist.dna(strm,model=model,gamma=gamma)
  dst[is.na(dst)] <- 0
  return(dst)
}
