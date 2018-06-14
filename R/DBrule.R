DBrule <-
function(grpDist,hr,oDist,g.names=NULL)
{ geovar <- function (D)
  { if(nrow(D)<2) 
      return(0)
    return(sum(D^2)/(2*nrow(D)^2))
  }
  phi <- function (d)
    sum(d^2)/ length (d)
  g <- max(as.integer(hr))
  grpDist <- as.matrix(grpDist)
  PHI2 <- vector(mode="numeric",length=g)
  names(PHI2) <- paste("Phi2",1:g,sep=".")
  if(!is.null(g.names))
    names(PHI2) <- paste("Phi2",g.names,sep=".")
  for(i in 1:g)
  { D <- grpDist[hr==i,hr==i,drop=FALSE]
    V <- geovar(D)
    PHI2[i] <- phi(oDist[hr==i])-V
  }
  idx <- match(min(PHI2),PHI2)
  clustName <- ifelse(!is.null(g.names),g.names[idx],idx)
  output <- list(Phi2=PHI2,DB.rule=idx,Type=clustName)
  return(output)
}
