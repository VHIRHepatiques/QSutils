\name{fn.ab}
\alias{fn.ab}
\title{
Vector of abundances with different methods
}
\description{
Function to simulate haplotype abundances in the quasispecies.
}
\usage{
fn.ab(n, h = 10000, r = 0.5,fn="pf")
fn.ab(n, h = 10000, r = 3,fn="pcf")
fn.ab(n, h = 10000, fn="dfp")
}
\arguments{
    \item{n}{
Number of counts to compute.
}
    \item{h}{
Highest abundance value.
}
    \item{r}{
A number to compute the abundance. See details. 
}
    \item{fn}{
Character indicating which function to use to compute the abundances 
"pf","pcf" or "dfp", see details. By default "pcf".
}
}
\details{
The abundances computed as a power of fractions, when fn is "pf", 
are computed according to the following equation, taking the integer part:
\deqn{max(h r^{(i-1)},1); ~~ 0<r<1;  ~~ i=1..n}
The lower \code{r}, the faster the decrease in abundance, \code{r} is in the
range 0 < r < 1.

With "pcf" the abundances are computed by a power of decreasing fractions, 
as counts, according to the following  equation, taking the integer part:
\deqn{max(h ~ \left( \frac{1}{i} \right) ^r,1); ~~ r>0; ~~i=1..n}
The higher \code{r,} the faster the decrease in abundances. In this case 
\code{r} corresponds to the power of the function, a value larger than 0,
usually in the range 0.5 < r < 4.

If fn is equal to "dfp", the abundances are computed by increasing root powers
according to the following equation,taking the integer part:
\deqn{ max(h^{(1/i)},1); ~~ i=1..n}
}
\value{
Numeric vector with \code{n} decreasing counts, where the first element 
equals \code{h}, and no element is lower than 1.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{geom.series}},\code{\link{GetRandomSeq}}, 
\code{\link{GenerateVars}},\code{\link{Diverge}}
}
\examples{
# Simulate a quasispecies alignment.
m1 <- GetRandomSeq(50)
v1 <- GenerateVars(m1,50,2,c(10,1)) 
qs <- c(m1,v1)
w_pf <- fn.ab(length(qs),h=1000,r=1.5,fn="pf")
w_pf
w_pcf <- fn.ab(length(qs),h=1000,r=1.5,fn="pcf")
w_pcf
w_dfp <- fn.ab(length(qs),h=1000,fn="dfp") 
w_dfp
}
