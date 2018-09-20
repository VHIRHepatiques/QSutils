\name{TotalMutations}
\alias{TotalMutations}
\title{
Number of Mutations
}
\description{
\code{TotalMutations} computes the number of mutations in the alignment.
}
\usage{
TotalMutations(hseqs,w)
}
\arguments{
    \item{hseqs}{
DNAStringSet or AAStringSet with the haplotype sequences.
}
    \item{w}{
An optional numeric vector with the haplotype counts used to compute the 
total number of mutations in the population, that is, taking into account
haplotype abundances. When \code{w} is NULL, a vector of ones is taken as 
default.
}
}
\value{
A value corresponding to the number of mutations. Note that the wild-type is 
decided taking \code{w} into account.
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
\code{\link{SegSites}}
}
\examples{

# Create the object.
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

TotalMutations(lst$hseqs)
TotalMutations(lst$hseqs,lst$nr)

}