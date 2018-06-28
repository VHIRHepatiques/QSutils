\name{Unknown-Genotype.fna}
\alias{Unknown-Genotype.fna}
\docType{data}
\title{
Fasta file with reads of unknown genotype. 
}
\description{
Fasta file of sequenced hepatitis virus of unknown genotype. This is toydata 
to simulate the genotyping of a sequence with the package \code{QSutils}. 
}
\format{
Fasta file, each sequence starts with te symbol ">" followed by the ID of 
the sequence. Subsequent lines correspond to the nucleotide sequences or 
peptide sequences.
}
\seealso{
\code{\link{DBrule}}
}
\examples{
lst2Geno <- ReadAmplSeqs("QSutils/extdata/Unknown-Genotype.fna",type="DNA")
hseq <- lst2Geno$hseq[1]
hseq
}