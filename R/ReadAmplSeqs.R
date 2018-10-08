ReadAmplSeqs <-
function(flnm,type="DNA"){ 
    if(type!="AA" & type!= "DNA") stop("Check the type input")
    if (type=="AA") seqs <- readAAStringSet(flnm)
    if (type=="DNA") seqs <- readDNAStringSet(flnm)
    nr <- vapply(names(seqs),function(str) strsplit(str,split="\\|")[[1]][2],
                character(1))
    nr <- as.numeric(nr)
    nr[is.na(nr)] <- 1
    return(list(nr=nr,hseqs=seqs))
}
