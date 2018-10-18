FAD <-
function(dst){
    ## Functional Attribute Diversity
        ##   dst: a distance object or a matrix of distances
    if(!is(dst, "dist") & !is(dst, "matrix"))
    stop("The input object must be of dist or matrix class \n")
    # Sum of all elements of the matrix
    return(sum(as.matrix(dst)))
}