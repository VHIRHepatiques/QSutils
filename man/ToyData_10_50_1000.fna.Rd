\name{ToyData_10_50_1000.fna}
\alias{ToyData_10_50_1000.fna}
\docType{data}
\title{
Fasta file with 10 haplotypes of 50 basepairs. 
}
\description{
Fasta file that contains the sequence of 10 haplotypes used as an 
example of the package \code{QSutils}.
}
\format{
Fasta file, each sequence starts with te symbol ">" followed by the 
ID of the sequence. Subsequent lines correspond to the nucleotide 
sequences or peptide sequences.
}
\examples{
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")
lst
}
