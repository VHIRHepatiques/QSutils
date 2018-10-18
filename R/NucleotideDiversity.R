NucleotideDiversity <-
function(dst, w=NULL){
    ##  Nucleotide diversity 
    ##  Computed as eq 10.5 in Nei (1987) pg.256
        ##     w: vector of counts
        ##   dst: a distance object or an aquare matrix of distances
    if(is.null(w))
        w <- rep(1, nrow(as.matrix(dst)))
    return(Rao(dst, w))
}