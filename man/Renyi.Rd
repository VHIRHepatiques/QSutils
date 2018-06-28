\name{Renyi}
\alias{Renyi}
\alias{RenyiProfile}
\title{
Renyi profiles.
}
\description{
Functions to compute the Rényi entropy given a vector of counts of a set of 
haplotypes. 
\code{RenyiProfile} computes the Rényi number for a set of exponents.
}
\usage{
Renyi(w, q)
RenyiProfile(w, q = NULL)
}
\arguments{
    \item{w}{
Vector of observed counts or frequencies.
}
    \item{q}{
Exponent. A single value for \code{Renyi}, a vector of values or NULL for
\code{RenyiProfile}.
}
}
\value{
A single value for \code{Renyi}. A data frame with exponents and entropies for
\code{RenyiProfile}.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. 
PubMed PMID: 24389655.

Pavoine, S. (2005). Méthodes statistiques pour la mesure de la biodiversité. 
UMR CNRS 5558 «Biométrie et Biologie Evolutive».
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{Hill}}, \code{\link{HCq}}
}
\examples{

# A vector of observed counts.
nr<-c(464, 62, 39, 27, 37, 16, 33, 54, 248, 20)

Renyi(nr,2)

RenyiProfile(nr,c(0:4,Inf))

RenyiProfile(nr)

}
