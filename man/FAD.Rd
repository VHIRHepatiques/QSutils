\name{FAD}
\alias{FAD}
\title{
Functional Attribute Diversity.
}
\description{
Computes the Functional Attribute Diversity as the sum of the pairwise distance matrix elements.
}
\usage{
FAD(dst)
}
\arguments{
  \item{dst}{
A "dist" object or a symmetric matrix with pairwise distances.
}
}
\value{
A value that corresponds to the Functional Attribute Diversity. The sum of matrix elements.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.
}
\author{
Josep Gregori and Mercedes Guerrero
}

\seealso{
\code{\link{DNA.dist}}
}
\examples{
\dontrun{

# Create the object.
lst <- ReadAmplSeqs("file.fasta")

# Compute the DNA distances matrix.
dst <- DNA.dist(lst$hseqs,model="N")

FAD(dst)

}}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line