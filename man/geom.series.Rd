\name{geom.series}
\alias{geom.series}
\title{
Geometric series
}
\description{
Function to simulate haplotype abundances in the quasispecies by
geometric series.
}
\usage{
geom.series(n,p=0.001)
}
\arguments{
    \item{n}{
Number of frequencies to compute.
}
    \item{p}{
Numeric parameter of the geometric function.
}
}
\details{
The abundances, as counts, are computed according to the following equation:
\deqn{p ~(1-p)^{i-1},  ~~ i=1..n}
The lower \code{r}, the faster the decrease in abundances.
}
\value{
Numeric vector with \code{n} decreasing counts.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{GetRandomSeq}}, \code{\link{GenerateVars}}, 
\code{\link{Diverge}}
}
\examples{
# Simulate a quasispecies alignment.
m1 <- GetRandomSeq(50)
v1 <- GenerateVars(m1,50,2,c(10,1)) 
qs <- c(m1,v1)
w <- geom.series(100,0.8)
}
