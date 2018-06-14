\name{GenerateVars}
\alias{GenerateVars}
\title{
Generate variants of a given haplotype.
}
\description{
Function to generate a set of variants for a given DNA sequence. 
}
\usage{
GenerateVars(seq, nhpl, max.muts, p.muts)
}
\arguments{
  \item{seq}{
A character string with a DNA sequence from which to generate the variants.
}
  \item{nhpl}{
Number of haplotypes to generate.
}
  \item{max.muts}{
Maximum number of mutations in each sequence.
}
  \item{p.muts}{
Vector of length \code{max.muts} with the probability of each number of mutations, 
some of which may be 0.
}
}
\details{
Given a DNA sequence, \code{nhpl} variant haplotypes are generated at random, 
with a maximum of \code{max.muts} substitutions each. The probability in the
number of mutations in each haplotype generated is given by the vector \code{p.muts}.
The positions of the mutations in each haplotype are independent and random.
}
\value{
A character vector with \code{nhpl} haplotype variants of the \code{seq} sequence. 
}
\author{
Josep Gregori and Mercedes Guerrero
}

\seealso{
\code{\link{GetRandomSeq}}, \code{\link{Diverge}}
}
\examples{
set.seed(123)
m1 <- GetRandomSeq(50)

GenerateVars(m1,50,2,c(10,1))
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line