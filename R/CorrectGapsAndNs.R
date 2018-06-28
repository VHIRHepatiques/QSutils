CorrectGapsAndNs <-
function(hseqs,ref.seq){ 
    if(class(hseqs)!="DNAStringSet" & class(hseqs)!="AAStringSet") 
        stop("The input object hseqs must be DNAStringSet or AAStringSet\n")
    if(class(ref.seq)!="character" & class(ref.seq)!="DNAString" & 
    class(ref.seq)!="AAString") 
        stop("The input object ref.seq must be of class character\n")
    CorrPos <- function(v,nt){ 
        fl <- v %in% c("-","N")
        v[fl] <- nt
        return (v)
    }
    rf <- strsplit(as.character(ref.seq),split ="" )[[1]]
    ntm <- as.matrix(hseqs)
    ntm <- sapply (1: length (rf), function (j) CorrPos(ntm[,j],rf[j]))
    if(class(hseqs)=="DNAStringSet") 
        correctseq <- Biostrings::DNAStringSet(apply(ntm,1,paste,collapse=""))
    if(class(hseqs)=="AAStringSet") 
        correctseq <- Biostrings::AAStringSet(apply(ntm,1,paste,collapse=""))
    return (correctseq)
}
