\name{ConsSeq}
\alias{ConsSeq}
\title{
Consensus sequence given an alignment and frequencies
}
\description{
ConsSeq determines the consensus sequence from a set of haplotypes.
}
\usage{
ConsSeq(seqs, w=NULL)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet object with the haplotype sequences.
}
    \item{w}{
An optional numeric vector with the haplotype counts.
}
}
\details{
The most frequent nucleotide or amino acid at each position is taken. No 
IUPAC ambiguity codes are considered; in the case of ties, the consensus
nucleotide is decided randomly.
}
\value{
Character vector with the consensus sequence.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

ConsSeq(lst$hseqs,lst$nr)
}