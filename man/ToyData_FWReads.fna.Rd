\name{ToyData_FWReads.fna}
\alias{ToyData_FWReads.fna}
\docType{data}
\title{
Fasta file with forward reads
}
\description{
Fasta file with forward strand reads. Toy data used to illustrate the 
intersections of forward and reverse haplotypes with the function 
\code{IntersectStrandHpls}.
}
\format{
Fasta file format. Each sequence starts with the symbol ">" followed by 
the sequence ID. Subsequent lines correspond to the nucleotide sequences or 
peptide sequences.

}
\seealso{
\code{\link{ToyData_RVReads.fna}}, \code{\link{IntersectStrandHpls}}
}
\examples{
filepath_FW<-system.file("extdata","ToyData_FWReads.fna", package="QSutils")
lstFW <- ReadAmplSeqs(filepath_FW,type="DNA")
filepath_RV<-system.file("extdata","ToyData_RVReads.fna", package="QSutils")
lstRV <- ReadAmplSeqs(filepath_RV,type="DNA")

lstI <- IntersectStrandHpls(lstFW$nr,lstFW$hseqs,lstRV$nr,lstRV$hseqs)
lstI
}