MutsTbl <-
function(hseqs,nr=NULL)
{ if(is.null(nr)) 
    nr <- rep(1,length(hseqs))
  seq.tbl <- FreqMat(hseqs,nr)
  j <- apply(seq.tbl,2,function(x) which.max(x)[1])
  seq.tbl[cbind(j,1:ncol(seq.tbl))] <- 0
  return(seq.tbl)
}
