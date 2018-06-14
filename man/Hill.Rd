\name{Hill}
\alias{Hill}
\alias{HillProfile}
\title{
Hill numbers.
}
\description{
Functions to compute Hill numbers.
\code{Hill} computes the Hill number of a single q value.
\code{HillProfile} computes the Hill numbers for all elements in vector \code{q}.
}
\usage{
Hill(w, q)
HillProfile(w, q = NULL)
}

\arguments{
  \item{w}{
Vector of observed counts or frequencies.
}
  \item{q}{
Exponent. 
}
}
\details{
In \code{Hill} just the first element in q is considered. 
\code{HillProfile} is vectorized and considers all elements in \code{q}, 
in this case when \code{q} is NULL a default vector is taken to get the full
profile in the range 0 to Inf.
}
\value{
A value or vector of values that correspond to the Hill number estimators of passed exponents.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. PubMed 
PMID: 24389655.

}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{HCq}}, \code{\link{Renyi}}
}
\examples{
# Vector of observed counts.
nr<-c(464, 62, 39, 27, 37, 16, 33, 54, 248, 20)

# Hill numbers of order 2.
Hill(nr,2)

# Set of most common values.
HillProfile(nr,q=c(0:4,Inf))

# Full Hill numbers profile.
HillProfile(nr)
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line