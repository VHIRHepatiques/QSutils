DSFT <-
function(nr,sz,p.cut=0.002,conf=0.95)
{ dsnr <- nr
  if(sum(nr)>size)
    dsnr <- round(nr/sum(nr)*size)
  thr <- qbinom(conf,sz,p.cut)
  return(dsnr >= thr)
}
