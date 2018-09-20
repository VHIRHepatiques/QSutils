\name{PolyDist}
\alias{PolyDist}
\title{
Fraction of substitutions by polymorphic site
}
\description{
Computes the fraction of substitutions at each polymorphic site. The wild-type
base is taken as the most abundant at each site, taking into account the 
weights, \code{w}.
}
\usage{
PolyDist(seqs,w=NULL)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet with the haplotype sequences.
}
    \item{w}{
An optional numeric vector with the haplotype counts. When \code{w} is NULL, 
the same weight is given to each haplotype.
}
}
\value{
Vector of numbers corresponding to the fraction of substitutions at polymorphic
sites. Note that the wild type also depends on \code{w}.
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
\code{\link{ReadAmplSeqs}}
}
\examples{

# Load haplotype alignment with abundances.
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

PolyDist(lst$hseqs) 
PolyDist(lst$hseqs,lst$nr)

}