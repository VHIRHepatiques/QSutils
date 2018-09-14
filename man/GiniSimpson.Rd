\name{GiniSimpson}
\alias{GiniSimpson}
\alias{GiniSimpsonMVUE}
\alias{GiniSimpsonVar}
\title{
Functions to calculate the GiniSimpson index
}
\description{
GiniSimpson calculates the unbiased estimator, GiniSimpsonVar computes 
Gini-Simpson asymp- totic variance, and GiniSimpsonMVUE calculates the minimum 
variance unbiased estimator of the Gini-Simpson index.

}
\usage{
GiniSimpson(w)
GiniSimpsonMVUE(w)
GiniSimpsonVar(w)
}
\arguments{
    \item{w}{
Vector of observed counts or frequencies.
}
}
\value{
A value that corresponds to the Gini-Simpson diversity index.
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

# A vector of haplotype counts.
nr <- c(464, 62, 39, 27, 37, 16, 33, 54, 248, 20)

# Gini-Simpson index.
GiniSimpson(nr)

# Gini-Simpson variance. 
GiniSimpsonVar(nr)

# MVUE Gini-Simpson index. 
GiniSimpsonMVUE(nr)
}