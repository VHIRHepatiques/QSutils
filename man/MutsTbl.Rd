\name{MutsTbl}
\alias{MutsTbl}
\title{
Table of mutation frequencies by position
}
\description{
Computes the table of mutation frequencies by position with respect to the 
alignment consensus.
}
\usage{
MutsTbl(hseqs,nr=NULL)
}
\arguments{
    \item{hseqs}{
DNAStringSet or AAStringSet with the aligned haplotype sequences.
}
    \item{nr}{
An optional numeric vector with the haplotype counts. When \code{nr} is NULL,
the same weight is given to each haplotype.
}
}
\value{
Matrix of mutation counts by position. A (4 x n) or (20 x n) matrix, 
where n is the alignment length.


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
# Load the haplotypes alignment with abundances.
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

# Table of mutations in the alignment, regardless of haplotype abundance. 
MutsTbl(lst$hseqs)

# Table of mutations taking into account abundance.
MutsTbl(lst$hseqs,lst$nr)
}