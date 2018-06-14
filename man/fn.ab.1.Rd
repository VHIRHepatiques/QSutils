\name{fn.ab.1}
\alias{fn.ab.1}
\title{
Vector of abundances by sequential powers of a fraction.
}
\description{
Function to simulate haplotype abundances of quasispecies by sequential powers of a fraction.
}
\usage{
fn.ab.1(n, h = 10000, r = 0.5)
}
\arguments{
  \item{n}{
Number of counts to compute.
}
  \item{h}{
Higher value for the counts.
}
  \item{r}{
A number in the range \code{0 < r < 1}.
}
}
\details{
The abundances, as counts, are computed according to the following equation, taking the integer part:
\deqn{max(h r^{(i-1)},1); ~~ 0<r<1;  ~~ i=1..n}
The lower \code{r} the faster the decrease in abundances.
}
\value{
Numeric vector with \code{n} decreasing counts, where the first element equals 
\code{h}, and no element is lower than 1.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{fn.ab.2}}, \code{\link{fn.ab.3}}, \code{\link{GetRandomSeq}},
\code{\link{GenerateVars}}, \code{\link{Diverge}}
}
\examples{
# Simulate a quasispecies alignment.
m1 <- GetRandomSeq(50)
v1 <- GenerateVars(m1,50,2,c(10,1)) 
qs <- c(m1,v1)
w <- fn.ab.1(length(qs),h=1000,r=1.5) 
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line