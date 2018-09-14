\name{Diverge}
\alias{Diverge}
\title{
Generate a set of diverging haplotypes
}
\description{
Generates a set of diverging haplotypes from the given DNA sequence. The 
haplotypes produced share a pattern of divergence with an increasing number 
of mutations.
}
\usage{
Diverge(vm, seq)
}
\arguments{
    \item{vm}{
Vector with number of diverging mutations to be generated.
}
    \item{seq}{
Reference sequence from which to generate the variants.
}
}
\details{
\code{max(vm)} Positions in the given sequence are randomly generated.
A substitution is also randomly produced for each of these positions. 
A haplotype is generated for each element in \code{vm}, so that it contains
\code{vm[i]} substitutions of those previously generated.
}
\value{
Character string vector with the segregating haplotypes generated.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{GetRandomSeq}} 
}
\examples{
set.seed(123)
m1 <- GetRandomSeq(50)
hpl <- Diverge(3:6,m1)
DottedAlignment(DNAStringSet(hpl))
}
