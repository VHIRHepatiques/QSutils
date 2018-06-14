\name{GetRandomSeq}
\alias{GetRandomSeq}
\title{
Generate a random sequence.
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
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{GenerateVars}}, \code{\link{Diverge}}
}
\examples{
set.seed(123)
GetRandomSeq(50)
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line