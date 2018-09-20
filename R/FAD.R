FAD <-
function(dst){
    if(!is(dst,"dist") & !is(dst,"matrix"))
    stop("The input object must be of dist or matrix class.\n")
    return(sum(as.matrix(dst)))
}