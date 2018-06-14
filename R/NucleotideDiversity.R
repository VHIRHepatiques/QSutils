NucleotideDiversity <-
function(dst,w=NULL)
{ if(is.null(w)) w <- rep(1,nrow(as.matrix(dst)))
  return(Rao(dst,w)) 
}