\name{Unknown-Genotype.fna}
\alias{Unknown-Genotype.fna}
\docType{data}
\title{
Fasta file with reads of unknown genotype
}
\description{
Fasta file with hepatitis B virus sequences of unknown genotype. This is used 
to illustrate the genotyping of HBV sequences with the \code{QSutils} package.

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
lst2Geno <- ReadAmplSeqs("QSutils/extdata/Unknown-Genotype.fna",type="DNA")
hseq <- lst2Geno$hseq[1]
hseq
}