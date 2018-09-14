\name{GenotypeStandards_A-H.fas}
\alias{GenotypeStandards_A-H.fas}
\docType{data}
\title{
Genotype standards of hepatitis B virus
}
\description{
Fasta file with a set of well characterized sequences belonging to each 
HBV genotype. See the QSutils vignette:
\code{vignette("QSutils", package = "QSutils")}.
}
\format{
Fasta file format. Each sequence starts with the symbol ">" followed by 
the sequence ID. Subsequent lines correspond to the nucleotide sequences or 
peptide sequences.
}
\seealso{
\code{\link{DBrule}}
}
\examples{
lstRefs <-  ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")
RefSeqs <- lstRefs$hseq
}
