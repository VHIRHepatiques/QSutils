\name{MutationFreq}
\alias{MutationFreq}
\alias{MutationFreqVar}
\title{
Mutation frequency with respect to the dominant haplotype
}
\description{

\code{MutationFreq} computes the mutation frequency given a vector of counts,
and the genetic distances of each haplotype to the dominant haplotype.
\code{MutationFreqVar} returns the variance of the mutation frequency.
}
\usage{
MutationFreq(dst=NULL,nm=NULL,nr=NULL,len=1)
MutationFreqVar(nm,nr=NULL,len=1)
}
\arguments{
    \item{dst}{
A "dist" object or a symmetric matrix with pairwise distances.
}
    \item{nm}{
Vector of distances or differences with respect to the dominant haplotype 
including itself (eg, nm[1] is 0 if w[1]==max(w)).
}
    \item{nr}{
An optional numeric vector with the haplotype counts.
}
    \item{len}{
The alignment width when nm is the number of differences, otherwise 1. Defaults
to 1.
}
}
\value{
A value corresponding to the mutation frequency for \code{MutationFreq} or 
its variance for \code{MutationFreqVar}. When \code{nr} is NULL, the same 
weight is given to each haplotype and the computed value corresponds to the
mutation frequency by entity.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. 
PubMed PMID: 24389655.
}

\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{DNA.dist}}, \code{\link{GetQSData}}, \code{\link{ReadAmplSeqs}}
}
\examples{

# Load alignment with abundances.
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- GetQSData(filepath,type="DNA")

# Mutation frequency. 
dst <- DNA.dist(lst$seqs,model="raw")
MutationFreq(dst=dst,len=width(lst$seqs)[1])

# Mutation frequency with abundances.
MutationFreq(nm=lst$nm,nr=lst$nr,len=width(lst$seqs)[1])

# Variance of the mutation frequency.
MutationFreqVar(nm=lst$nm,nr=lst$nr,len=width(lst$seqs)[1])
}