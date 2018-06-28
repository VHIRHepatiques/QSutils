\name{fn.ab.3}
\alias{fn.ab.3}
\title{
Vector of abundances by increasing root powers.
}
\description{
Function to simulate haplotype abundances of quasispecies 
by increasing root powers.
}
\usage{
fn.ab.3(n, h = 10000)
}
\arguments{
    \item{n}{
Number of counts to compute.
}
    \item{h}{
Higher value for the counts.
}
}
\details{
The abundances, as counts, are computed according to the following 
equation, taking the integer part:
\deqn{ max(h^{(1/i)},1); ~~ i=1..n}
}
\value{
Numeric vector with \code{n} decreasing counts, where 
the first element equals \code{h}, and no element is lower than 1.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{fn.ab.1}}, \code{\link{fn.ab.2}}, 
\code{\link{geom.series}}, \code{\link{GetRandomSeq}},
\code{\link{GenerateVars}}, \code{\link{Diverge}}
}
\examples{
# Simulate a quasispecies alignment.
m1 <- GetRandomSeq(50)
v1 <- GenerateVars(m1,50,2,c(10,1)) 
qs <- c(m1,v1)
w <- fn.ab.3(length(qs),h=1000) 
}
