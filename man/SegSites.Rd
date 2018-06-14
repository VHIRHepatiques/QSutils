\name{SegSites}
\alias{SegSites}
\title{
Computes the number of segregating sites.
}
\description{
Computes the number of segregating (polymorphic) sites given an alignment. 
That is, the number of sites with more than a single nucleotide or amino acid in the
alignment.
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
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. PubMed 
PMID: 24389655.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{
\dontrun{
# Create the object.
lst <- ReadAmplSeqs("file.fna")

SegSites(lst$hseqs)
}}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line