ReportVariants <-
function(hseqs, ref.seq, nr=NULL, start=1){ 
    ## Reports the variants of an aligment of haplotypes given a
    ## reference sequence.
        ##   hseqs: the aligned haplotypes
        ## ref.seq: reference sequence of the alignment
        ##      nr: vector of counts
        ##   start: position of the first nucleotide in the alignment
    if(!is(hseqs, "DNAStringSet") & !is(hseqs, "AAStringSet"))
        stop("The input object hseqs must be DNAStringSet  or AAString\n")
    if(is.null(nr)) nr<- rep(1, length(hseqs))
    if(!is(nr, "numeric")) stop("The imput object nr must be numeric \n")
    if(!is(ref.seq, "character") & !is(ref.seq, "DNAString")
    & !is(ref.seq, "AAString"))
        {stop("The input object ref.seq must be character \n")}
    # Split DNAString ref.seq in bases
    rnt <- strsplit(as.character(ref.seq), split="")[[1]]
    # Split haplotypes DNAStrinSet in bases , as a matrix
    mnt <- as.matrix(hseqs)
    # Which sequences are variants?
    jdx <- which(vapply(seq_len(ncol(mnt)),function (j) 
        sum(mnt[,j] != rnt[j]) > 0, logical(1L)))
    # Iniciate the data.frame of the output
    k <- 0
    vars <- data.frame(WT=character(), Pos=numeric(), Var=character(),
    Cov=numeric(), stringsAsFactors=FALSE)
    # Fill the output vars whith the info of each variant
    for(j in jdx){ 
        idx <- which(mnt[,j] != rnt[j])
        vnr <- tapply(nr[idx], mnt[idx,j], sum)
        for (i in seq_len(length(vnr))){ 
            k <- k+1
            vars[k,"WT"]  <- rnt[j]
            vars[k,"Pos"] <- j+start-1
            vars[k,"Var"] <- names(vnr)[i]
            vars[k,"Cov"] <- vnr[i]
        }
    }
    return(vars)
}
