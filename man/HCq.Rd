\name{HCq}
\alias{HCq}
\alias{HCqVar}
\alias{HCqProfile}
\title{
Set of functions to compute the Havrda-Charvat estimator
}
\description{
HCq computes the Havrda-Charvat estimator, and HCqVar computes the 
Havrda-Charvat asymptotic variance for a given exponent. By using HCqProfile, 
a Havrda-Charvat estimator is calculated for a predefined vector of exponents 
to obtain the full profile in the range, 0 to Inf.

}
\usage{
HCq(w, q)
HCqVar(w, q)
HCqProfile(w, q = NULL)
}
\arguments{
    \item{w}{
Vector of observed counts or frequencies.
}
    \item{q}{
Exponent. By default, a vector of values 1, 2, 3, 4 and Inf.
}
}
\details{
In \code{HCq} only  the first element in q is considered. \code{HCqProfile} is 
vectorized and considers all elements in q. When q is NULL: in this case, a 
default vector is taken to obtain the full profile in the range 0 to Inf.
}
\value{
A value that corresponds to the Havrda-Charvat estimator when \code{HCq} or 
\code{HCqVar} is used. A data frame with the Havrda-Charvat estimator for each 
exponent when \code{HCqProfile} is used.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. 
PubMed PMID: 24389655.

Pavoine, S. (2005). M?thodes statistiques pour la mesure de la biodiversit?. 
UMR CNRS 5558 Biometrie et Biologie Evolutive.

}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{Hill}}, \code{\link{Renyi}}
}
\examples{

# A vector of observed counts.
nr<-c(464, 62, 39, 27, 37, 16, 33, 54, 248, 20)

# Havrda-Charvat estimator for q=4.
HCq(nr,4)

# Havrda-Charvat estimator variance for q=4.
HCqVar(nr,4)

# Prolife of Havrda-Charvat estimator for 0:4 and Inf.
HCqProfile(nr,c(0:4,Inf))

# Full profile.
HCqProfile(nr)
}