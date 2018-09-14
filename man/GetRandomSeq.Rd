\name{GetRandomSeq}
\alias{GetRandomSeq}
\title{
Generate a random sequence
}
\description{
Creates a random DNA sequence of a given length.
}
\usage{
GetRandomSeq(seq.len)
}
\arguments{
    \item{seq.len}{
The sequence length.
}
}
\value{
A character string representing a DNA sequence.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{GenerateVars}}, \code{\link{Diverge}}
}
\examples{
set.seed(123)
GetRandomSeq(50)
}