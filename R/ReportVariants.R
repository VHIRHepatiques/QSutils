ReportVariants <-
function(hseqs,ref.seq,nr=NULL,start=1){ 
    if(!is(hseqs,"DNAStringSet") & !is(hseqs,"AAStringSet"))
        stop("The input object hseqs must be DNAStringSet  or AAString\n")
    if(is.null(nr)) nr<- rep(1,length(hseqs))
    if(!is(nr,"numeric")) stop("The imput object nr must be numeric \n")
    if(!is(ref.seq,"character") & !is(ref.seq,"DNAString")
    & !is(ref.seq,"AAString"))
        {stop("The input object ref.seq must be character \n")}
    rnt <- strsplit(as.character(ref.seq),split="")[[1]]
    mnt <- as.matrix(hseqs)
    jdx <- which(sapply(1:ncol(mnt),function (j) sum(mnt[,j]!=rnt[j])>0))
    k <- 0
    vars <- data.frame(WT=character(),Pos=numeric(),Var=character(),
    Cov=numeric(),stringsAsFactors=FALSE)
    for(j in jdx){ 
        idx <- which(mnt[,j]!=rnt[j])
        vnr <- tapply(nr[idx],mnt[idx,j],sum)
        for (i in 1: length(vnr)){ 
            k <- k+1
            vars[k,"WT"]  <- rnt[j]
            vars[k,"Pos"] <- j+start-1
            vars[k,"Var"] <- names(vnr)[i]
            vars[k,"Cov"] <- vnr[i]
        }
    }
    return(vars)
}
