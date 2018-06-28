\name{SortByMutations}
\alias{SortByMutations}
\title{
Sorts haplotypes by mutations and abundance.
}
\description{
Sorts and renames the haplotypes by the number of mutations with 
respect to the dominant haplotype, and by abundance.
}
\usage{
SortByMutations(bseqs, nr)
}
\arguments{
    \item{bseqs}{
DNAStringSet or AAStringSet object with the haplotype alignment.
}
    \item{nr}{
Vector with the haplotype counts.
}
}
\details{
The haplotypes are pairwise aligned to the dominant haplotype and then sorted
first by decreasing order in the number of differences with respect to the
dominant haplotype, and second by decreasing order of abundances. As a result
the haplotypes are renamed according to the pattern \code{Hpl.n.xxxx}, 
where \code{n} represents the number of differences, and \code{xxxx} the
abundance order within mutation number.
}

\value{
Returns a list with three elements.
\item{bseqs}{DNAStringSet or AAStringSet with the haplotype sequences.}
\item{nr}{Vector of the haplotype counts.}
\item{nm}{Vector of the number of differences of each haplotype with respect 
to the dominant haplotype.}
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
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{

# Load haplotypes alignment with abundances.
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

SortByMutations(lst$hseq,lst$nr)
}