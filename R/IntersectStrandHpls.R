.AlgnStrandHpls <-
    function (nrFW , hseqsFW ,nrRV , hseqsRV ){ 
        ## Helper function to obtain the aligned 
        ##   haplotipes of FW and RV reads
        names(nrFW) <- as.character(hseqsFW)
        names(nrRV) <- as.character(hseqsRV)
        # Reads present in FW and RV
        nms <- union(as.character(hseqsFW),as.character(hseqsRV))
        nb <- length(nms)
        # Filter FW reads in nms
        pFW <- rep(0,nb)
        FWseq<-as.character(unlist(DNAStringSetList(hseqsFW)))
        idx <- which(nms %in% FWseq)
        pFW[idx] <- nrFW[nms[idx]]
        # Filter RV reads in nms
        pRV <- rep(0,nb)
        RVseq<-as.character(unlist(DNAStringSetList(hseqsRV)))
        idx <- which(nms %in% RVseq )
        pRV[idx] <- nrRV[nms[idx]]
        return(list(pFW=pFW,pRV=pRV,Hpl=nms))
    }
IntersectStrandHpls <-
    function (nrFW , hseqsFW ,nrRV , hseqsRV , thr =0.001){ 
        ## Forward and reverse strand haplotype intersections
            ##      nrFW : forward strand haplotypes counts
            ##   hseqsFW : forward strand haplotypes
            ##      nrRV : reverse strand haplotypes counts
            ##   hseqsRV : reverse strand haplotypes
        if(length(nrFW)!= length(hseqsFW)) 
            stop("The length of the sequences and the counts must be equal \n")
        if(length(nrRV)!= length(hseqsRV)) 
            stop("The length of the sequences and the counts must be equal \n")
        if(!is(hseqsFW, "character") & !is(hseqsRV, "character") & 
            !is(hseqsFW, "DNAStringSet") & !is(hseqsRV, "DNAStringSet") &
            !is(hseqsFW, "AAStringSet") & !is(hseqsRV, "AAStringSet")) 
        {stop("The sequences must be character vector or DNAStringSet or 
            AAStringSet\n")}
        if(!is(nrFW, "numeric") & !is(nrRV, "numeric") &
            !is(thr, "numeric"))
        {stop("The sequences must be numeric vector \n")}
        # Filter those sequences that does not reach the threshold
        flFW <- nrFW /sum(nrFW) >= thr
        flRV <- nrRV /sum (nrRV) >= thr
        # Align forward and reverse reads
        lst <- .AlgnStrandHpls ( nrFW[flFW], hseqsFW[flFW],
                                nrRV[flRV], hseqsRV[flRV])
        # Haplotipes present in both strands
        fl <- lst$pFW > 0 & lst$pRV > 0
        # Haplotipe sequences
        hseqs <- lst$Hpl[fl]
        # Haplotipe abundance
        nr <- sum(lst$pFW[fl]+lst$pRV[fl])
        o <- order(nr, decreasing = TRUE)
        return(list(hseqs=DNAStringSet(hseqs[o]),nr=nr[o],pFW=lst$pFW,
                    pRV=lst$pRV))
    }