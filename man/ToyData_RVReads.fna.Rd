\name{ToyData_RVReads.fna}
\alias{ToyData_RVReads.fna}
\docType{data}
\title{
Fasta file with reverse reads. 
}
\description{
Fasta file with reverse strand reads. Toydata to be used to intersect the 
forward and reverse haplotypes with the function \code{IntersectStrandHpls}.
}
\format{
Fasta file, each sequence starts with te symbol ">" followed by the ID of the 
sequence. Subsequent lines correspond to the nucleotide sequences or peptide
sequences.
}
\seealso{
\code{\link{ToyData_FWReads.fna}}, \code{\link{IntersectStrandHpls}}
}
\examples{
lstFW <- ReadAmplSeqs("QSutils/extdata/ToyData_FWReads.fna",type="DNA")
lstRV <- ReadAmplSeqs("QSutils/extdata/ToyData_RVReads.fna",type="DNA")
lstI <- IntersectStrandHpls(lstFW$nr,lstFW$hseqs,lstRV$nr,lstRV$hseqs)
lstI
}