\name{GenotypeStandards_A-H.fas}
\alias{GenotypeStandards_A-H.fas}
\docType{data}
\title{
Genotype Standards of Hepatitis virus.  
}
\description{
Fasta file with the references to genotype sequences of hepatitis
virus into the 8 known haplotypes. For more details see the 
QSutils vignette: \code{vignette("QSutils", package = "QSutils")}
}
\format{
Fasta file, each sequence starts with te symbol ">" followed by the 
ID of the sequence. Subsequent lines correspond to the nucleotide 
sequences or peptide sequences.
}
\seealso{
\code{\link{DBrule}}
}
\examples{
lstRefs <-  ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")
RefSeqs <- lstRefs$hseq
}
