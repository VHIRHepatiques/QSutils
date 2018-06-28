\name{fn.ab.2}
\alias{fn.ab.2}
\title{
Vector of abundances by a power of decreasing fractions.
}
\description{
Function to simulate haplotype abundances of quasispecies by 
a power of decreasing fractions.
}
\usage{
fn.ab.2(n, h = 10000, r = 3)
}
\arguments{
    \item{n}{
Number of counts to compute.
}
    \item{h}{
Higher value for the counts.
}
    \item{r}{
The power of the function. A value bigger than 0, usually 
in the range \code{0.5 < r < 4}.
}
}
\details{
The abundances, as counts, are computed according to the 
following equation, taking the integer part:
\deqn{max(h ~ \left( \frac{1}{i} \right) ^r,1); ~~ r>0; ~~i=1..n}
The higher \code{r} the faster the decrease in abundances.
}
\value{
Numeric vector with \code{n} decreasing counts, where the
first element equals \code{h}, and no element is lower than 1.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{fn.ab.1}}, \code{\link{fn.ab.3}}, 
\code{\link{geom.series}}, \code{\link{GetRandomSeq}}, 
\code{\link{GenerateVars}}, \code{\link{Diverge}}
}
\examples{
# Simulate a quasispecies alignment.
m1 <- GetRandomSeq(50)
v1 <- GenerateVars(m1,50,2,c(10,1)) 
qs <- c(m1,v1)
w <- fn.ab.2(length(qs),h=1000,r=1.5) 
}