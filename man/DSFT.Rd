\name{DSFT}
\alias{DSFT}
\title{
Donsampling followed by fringe trimming.
}
\description{
Diversity indices are more or less influenced by the sample size on  which they were computed. This function helps in minimizing the bias tied to a sample size. First the vector of abundances is scaled to a lower sample size, then all haplotypes with abundances below a given threshold are excluded with high confidence.
}
\usage{
DSFT(nr, sz, p.cut = 0.002, conf = 0.95)
}
\arguments{
  \item{nr}{
Vector of observed haplotype counts.
}
  \item{sz}{
Size to downsample.
}
  \item{p.cut}{
Abundance threshold.
}
  \item{conf}{
Confidence in trimming.
}
}
\value{
Vector of logicals, with false the haplotypes to be excluded.
}
\references{
2013,2016
}
\author{
Josep Gregori and Mercedes Guerrero
}

\examples{

}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
