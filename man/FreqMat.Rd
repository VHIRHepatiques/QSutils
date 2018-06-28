\name{FreqMat}
\alias{FreqMat}
\title{
Matrix of nucleotide or amino acid frequencies in alignment by position.
}
\description{
Computes the nucleotide or amino acid frequencies at each position in
the alignment. 
}
\usage{
FreqMat(seqs,nr=NULL)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet with the aligned haplotype sequences.
}
    \item{nr}{
An optional numeric vector with the haplotype counts to be used to take
into account the haplotype frequencies.
}
}
\value{
Matrix with the frequency of each nucleotide or amino acid in each position.
A (4 x n) or (20 x n) matrix, where n is the alignment length.
}

\author{
Josep Gregori and Mercedes Guerrero
}
\examples{

lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

# Frequencies just in the alignment.
FreqMat(lst$hseqs)
# Taking also into account haplotype frequencies.
FreqMat(lst$hseqs,lst$nr)
}