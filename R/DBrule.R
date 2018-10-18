.geovar <- function(D){ 
    if(nrow(D) < 2) 
        return(0)
    return(sum(D^2)/(2*nrow(D)^2))
}
.phi <- function (d){
    ## Function to compute Phi2
    sum(d^2)/ length(d)
}
    DBrule <-
function(grpDist, hr, oDist, g.names=NULL){ 
    ## Genotyping by the DB rule
        ## grpDist: Distances among reference sequences
        ##      hr: Factor or a vector of integers that contains the 
        ##           type or subtype for each reference sequence
        ##   oDist: distance from the sequence to be classified to the
        ##          reference sequences.
        ## g.names: type or subtype names, defaults to its index .
    # How many genotypes are?
    g <- max(as.integer(hr))
    # Convert the distances into matrix.
    grpDist <- as.matrix(grpDist)
    # Initiating vector of Phi2 and nameing the elements.
    PHI2 <- vector(mode="numeric", length=g)
    if(!is.null(g.names)){
        names(PHI2) <- paste("Phi2", g.names, sep=".")
    } else {
        names(PHI2) <- paste("Phi2", seq_len(g), sep=".")
    }
    # Compute the Phi2 of target-references.
    for(i in seq_len(g)){ 
        D <- grpDist[hr == i, hr == i, drop=FALSE]
        V <- .geovar(D)
        PHI2[i] <- .phi(oDist[hr == i])-V
    }
    # What is the minimum distance? 
    idx <- match(min(PHI2), PHI2)
    clustName <- ifelse(!is.null(g.names), g.names[idx], idx)
    output <- list(Phi2=PHI2, DB.rule=idx, Type=clustName)
    return(output)
}
