CorrectGapsAndNs <-
function(hseqs,ref.seq){ 
    if(!is(hseqs,"DNAStringSet") & !is(hseqs,"AAStringSet"))
        stop("The input object hseqs must be DNAStringSet or AAStringSet\n")
    if(!is(ref.seq,"character") & !is(ref.seq,"DNAString") & 
    !is(ref.seq,"AAString")) 
        stop("The input object ref.seq must be of class character\n")
    CorrPos <- function(v,nt){ 
        fl <- v %in% c("-","N")
        v[fl] <- nt
        return (v)
    }
    rf <- strsplit(as.character(ref.seq),split ="" )[[1]]
    ntm <- as.matrix(hseqs)
    ntm <- vapply(seq_len(length(rf)), function (j) CorrPos(ntm[,j],rf[j]),
                character(nrow(ntm)))
    if(is(hseqs,"DNAStringSet")) 
        correctseq <- Biostrings::DNAStringSet(apply(ntm,1,paste,collapse=""))
    if(is(hseqs,"AAStringSet"))
        correctseq <- Biostrings::AAStringSet(apply(ntm,1,paste,collapse=""))
    return (correctseq)
}
