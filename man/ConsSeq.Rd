\name{ConsSeq}
\alias{ConsSeq}
\title{
Consensus sequence given an alignment and frequencies.
}
\description{
ConsSeq determines the consensus sequence from a set of hapotypes.
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
The most frequent nucleotide or amino acid at each position is taken. 
No ambiguity IUPAC codes are considered; in case of ties the consensus
is decided randomly.
}
\value{
Character vector with the consensus sequence.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{

lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

ConsSeq(lst$hseqs,lst$nr)
}