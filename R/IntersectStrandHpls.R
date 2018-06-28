IntersectStrandHpls <-
function (nrFW , hseqsFW ,nrRV , hseqsRV , thr =0.001){ 
    if(length(nrFW)!= length(hseqsFW)) 
        stop("The length of the sequences and the counts must be equal \n")
    if(length(nrRV)!= length(hseqsRV)) 
        stop("The length of the sequences and the counts must be equal \n")
    if(class(hseqsFW)!= "character" & class(hseqsRV)!= "character" & 
        class(hseqsFW)!= "DNAStringSet" & class(hseqsRV)!= "DNAStringSet") 
        {stop("The sequences must be character vector or DNAStringSet\n")}
    if(class(nrFW)!="numeric" & class(nrRV)!="numeric" & class(thr)!="numeric") 
        {stop("The sequences must be numeric vector \n")}
    AlgnStrandHpls <-
    function (nrFW , hseqsFW ,nrRV , hseqsRV ){ 
        names(nrFW) <- hseqsFW
        names(nrRV) <- hseqsRV
        nms <- union(hseqsFW,hseqsRV)
        nb <- length(nms)
        pFW <- rep(0,nb)
        FWseq<-as.character(unlist(DNAStringSetList(hseqsFW)))
        idx <- which(nms %in% FWseq)
        pFW[idx] <- nrFW[nms[idx]]
        pRV <- rep(0,nb)
        RVseq<-as.character(unlist(DNAStringSetList(hseqsRV)))
        idx <- which(nms %in% RVseq )
        pRV[idx] <- nrRV[nms[idx]]
        return(list(pFW=pFW,pRV=pRV,Hpl=nms))
    }
    flFW <- nrFW /sum(nrFW) >= thr
    flRV <- nrRV /sum (nrRV) >= thr
    lst <- AlgnStrandHpls ( nrFW[flFW], hseqsFW[flFW],
    nrRV[flRV], hseqsRV[flRV])
    fl <- lst $pFW > 0 & lst $pRV > 0
    hseqs <- lst$Hpl[fl]
    nr <- sum(lst$pFW[fl]+lst$pRV[fl])
    o <- order(nr, decreasing = TRUE)
    return(list(hseqs=hseqs[o],nr=nr[o],pFW=lst$pFW,pRV=lst$pRV))
}
