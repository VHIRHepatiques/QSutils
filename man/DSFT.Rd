\name{DSFT}
\alias{DSFT}
\title{
Downsampling followed by fringe trimming
}
\description{
Diversity indices are influenced to a greater or lesser degree by the sample 
size on which they are computed. This function helps to minimize the bias 
inherent to sample size. First the vector of abundances is scaled to a smaller
sample size, then all haplotypes with abundances below a given threshold are 
excluded with high confidence.
}
\usage{
DSFT(nr, size, p.cut = 0.002, conf = 0.95)
}
\arguments{
    \item{nr}{
Vector of observed haplotype counts.
}
    \item{size}{
Size to downsample.
}
    \item{p.cut}{
Abundance threshold.
}
    \item{conf}{
Confidence in trimming.
}
}
\value{
Vector of logicals, with false the haplotypes to be excluded. 
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2.
PubMed PMID: 24389655.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}

\examples{
# Generate viral quasispecies abundance data.
set.seed(123)
n <- 2000
y <- geom.series(n,0.8)+geom.series(n,0.0004)
nr.pop <- round(y*1e7)
# Get a sample of 10000 reads from this population.
sz2 <- 10000
nr.sz2 <- table(sample(length(nr.pop),size=sz2,replace=TRUE,p=nr.pop))
# Filter out haplotypes below 0.1%.
thr <- 0.1
fl <-  nr.sz2>=sz2*thr/100
nr.sz2 <- nr.sz2[fl]
Shannon(nr.sz2) #0.630521
# DSFT to 5000 reads.
sz1 <- 5000
fl <- DSFT(nr.sz2,sz1)
nr.sz2 <- nr.sz2[fl]
# Compute size corrected Shannon entropy.
Shannon(nr.sz2) #0.6189798
}