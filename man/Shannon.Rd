\name{Shannon}
\alias{Shannon}
\alias{ShannonVar}
\alias{NormShannon}
\alias{NormShannonVar}
\title{
Set of functions to compute Shannon entropy
}
\description{
\code{Shannon} computes the Shannon entropy. 
\code{NormShannon} returns the normalized Shannon entropy. 
\code{ShannonVar} computes the Shannon entropy asymptotic variance. 
\code{NormShannonVar} computes the normalized Shannon entropy asymptotic 
variance.
}
\usage{
Shannon(w)
ShannonVar(w)
NormShannon(w)
NormShannonVar(w)
}
\arguments{
    \item{w}{
Vector of observed counts or frequencies.
}
}
\value{
A single value with the result of the computations.
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

# Create a vector of observed counts.
nr<-c(464, 62, 39, 27, 37, 16, 33, 54, 248, 20)

# Shannon entropy.
Shannon(nr)

# Shannon entropy variance.
ShannonVar(nr)

# Normalized Shannon entropy.
NormShannon(nr)

# Normalized Shannon entropy variance.
NormShannonVar(nr)

}