MutationFreq <-
function(nm,nr=NULL,len=1)
{ if(is.null(nr)){
    if(class(nm)!="dist") stop("The input object must be dist class \n")
      nru <- rep(1,nrow(as.matrix(nm)))
      mf <- sum(as.matrix(nm)[1,]*nru)/sum(nru)
} else {
  if(length(nm)!=length(nr)) stop("The inputs nr and nm must have the same length \n")
  mf <- sum(nm*nr/sum(nr))/len
  names(mf) <- NULL
}
  return(mf)
}
