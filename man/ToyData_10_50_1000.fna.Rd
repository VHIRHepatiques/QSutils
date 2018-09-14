\name{ToyData_10_50_1000.fna}
\alias{ToyData_10_50_1000.fna}
\docType{data}
\title{
Fasta file with 10 haplotypes, 50 basepairs in size.
}
\description{
Fasta file that contains the sequence of 10 haplotypes used as examples in 
the \code{QSutils} package.
}
\format{
Fasta file format. Each sequence starts with the symbol ">" followed by the 
sequence ID. Subsequent lines correspond to the nucleotide sequences or 
peptide sequences.

}
\examples{
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")
lst
}
