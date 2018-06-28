FAD <-
function(dst){ 
    if(class(dst)!="dist" & class(dst)!="matrix") 
    stop("The input object must be of dist or matrix class.\n")
    return(sum(as.matrix(dst)))
}