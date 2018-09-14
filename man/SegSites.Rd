\name{SegSites}
\alias{SegSites}
\title{
Compute the number of segregating sites
}
\description{
Computes the number of segregating (polymorphic) sites in a given alignment.
That is, the number of sites with more than a single nucleotide or amino acid 
in the alignment.
}
\usage{
SegSites(seqs)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet with the haplotype sequences.
}
}
\value{
A value corresponding to the number of polymorphic sites.
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

# Create the object.
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

SegSites(lst$hseqs)
}