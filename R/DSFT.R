DSFT <-
function(nr,size,p.cut=0.002,conf=0.95){ 
    ## Downsampling followed by fringe trimming
        ##    nr: Vector of observed haplotype counts
        ##  size: Size to downsample
        ## p.cut: Abundance threshold
        ##  conf: Confidence in trimming
    dsnr <- nr
    # Downsampling
    if(sum(nr) > size)
        dsnr <- round(nr/sum(nr)*size)
    # Fringe trimming
    thr <- qbinom(conf, size, p.cut)
    return(dsnr >= thr)
}
