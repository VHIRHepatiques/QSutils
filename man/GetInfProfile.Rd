\name{GetInfProfile}
\alias{GetInfProfile}
\title{
Information content profile of an alignment
}
\description{
GetInfProfile computes the information content at each position of an 
alignment.
}
\usage{
GetInfProfile(seqs,nr=NULL)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet with the haplotype alignment.
}
    \item{nr}{
An optional numeric vector with the haplotype counts to take into account the 
information content of each position in the alignment.
}
}
\value{
Returns a numeric vector whose length is equal to the length of the alignment.
Each value corresponds to the information content of each position in 
the alignment.
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

\examples{

# Load the alignment.
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

# Compute the alignment's IC profile.
GetInfProfile(lst$hseqs)
# Also taking into account haplotype frequencies.
GetInfProfile(lst$hseqs,lst$nr)
}
