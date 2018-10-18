MutationFreq <-
function(dst=NULL, nm=NULL, nr=NULL, len=1){ 
    ##  Computes the Mutation frequency 
        ##  dst: a distance object or an aquare matrix of distances
        ##   nr: vector of counts
        ##   nm: vector of distances or differences with respect to a reference
        ##  len: when nm is number of differences, the alignment width
    #Check if input is a matrix of distances or vector of counts.
    if(!is.null(dst) & is.null(nr)){
        #If the input is dst:
        if(!is(dst, "dist") & !is(dst, "matrix")) 
            stop("The input object must be dist or matrix class \n")
        nru <- rep(1, nrow(as.matrix(dst)))
        #Compute the mutation frequency
        mf <- sum(as.matrix(dst)[1,]*nru)/sum(nru)
    } else if (is.null(dst) & !is.null(nr)){
        # If the input is nr, nm and len: 
        if(length(nm) != length(nr)) 
            stop("The inputs nr and nm must have the same length \n")
        #Compute the mutation frequency
        mf <- sum(nm*nr/sum(nr))/len
        names(mf) <- NULL
    } else {
        stop("Input object should be dst OR nr, but never both.")
    }
    return(mf)
}
#---------------------------------------------------------------------------
MutationFreqVar <-
function(nm, nr=NULL, len=1){ 
    ##  Mutation frequency standard deviation (cf. M. Salicru)
        ##    w: vector of counts
        ##   nm: vector of distances or differences with respect to a reference
        ##  len: when nm is number of differences, the alignment width
    if(is.null(nr)) nr <- rep(1, length(nm))
    if(!length(nm) == length(nr)) 
        stop("The inputs nr and nm must have the same length \n")
    N <- sum(nr)
    if(N < 2) return(0)
    p <- nr/sum(nr)
    v <- ((sum(p*nm^2)-sum(p*nm)^2)/len^2) / N
    names(v) <- NULL
    return(v)
}

