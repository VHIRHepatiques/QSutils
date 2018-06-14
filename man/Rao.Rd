\name{Rao}
\alias{Rao}
\alias{RaoVar}
\alias{RaoPow}
\alias{RaoPowProfile}
\title{
Functions to compute Rao's entropy.
}
\description{
Set of functions to estimate the functional Rao entropy. 
\code{Rao} calculates the Rao's entropy, \code{RaoVar} the variance of the Rao
estimator, \code{RaoPow} the Rao entropy of order q, and \code{RaoPowProfile} 
the functional Rao entropy profile for the given set of exponents.
}
\usage{
Rao(dst, w=NULL)
RaoVar(dst,w=NULL)
RaoPow(dst,q,w=NULL)
RaoPowProfile(dst,w=NULL,q=NULL)
}

\arguments{
  \item{dst}{
A "dist" object, the output of DNA.dist function.
}
  \item{w}{
An optional numeric vector with the haplotype counts to be used to take into account the haplotype frequencies. When \code{w} is NULL the same weight is given to each haplotype.
}
  \item{q}{
Exponent. A single value for \code{Rao}, \code{RaoVar} and \code{RaoPow}. 
A vector of values for \code{RaoPowProfile}.
The default value for \code{RaoPowProfile} is a set of exponents to obtain a
smooth profile.
}
}
\value{
A single value for \code{Rao}, \code{RaoVar} and \code{RaoPow}.
A vector of values for \code{RaoPowProfile} corresponding to each exponent 
in vector q.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. PubMed 
PMID: 24389655.

Pavoine, S. (2005). Méthodes statistiques pour la mesure de la biodiversité. UMR CNRS 5558 «Biométrie et Biologie Evolutive».
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{DNA.dist}}, \code{\link{ReadAmplSeqs}}
}
\examples{
\dontrun{
# Load haplotypes alignment with abundances.
lst <- ReadAmplSeqs("file.fasta")
# DNA pairwise distances.
dst <- DNA.dist(lst$hseqs,model="N")

Rao(dst,lst$nr)
RaoVar(dst,lst$nr)
RaoPow(dst,2,lst$nr)
RaoPowProlife(dst,lst$nr,c(0:4,Inf))
RaoPowProlife(dst,lst$nr)
}}