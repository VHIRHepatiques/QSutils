\name{FAD}
\alias{FAD}
\title{
Functional Attribute Diversity.
}
\description{
Computes the Functional Attribute Diversity as the sum of the pairwise
distance matrix elements.
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
A value that corresponds to the Functional Attribute Diversity.
The sum of matrix elements.
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
# Create the object.
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

# Compute the DNA distances matrix.
dst <- DNA.dist(lst$hseqs,model="N")

FAD(dst)

}