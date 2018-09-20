\name{Toy.GapsAndNs.fna}
\alias{Toy.GapsAndNs.fna}
\docType{data}
\title{
Fasta file with raw reads with gaps and Ns
}
\description{
Fasta file of sequenced data with some missing information. This is toy data 
to illustrate some functions of the package \code{QSutils} package. 
}
\format{
Fasta file format. Each sequence starts with the symbol ">" followed by the 
sequence ID. Subsequent lines correspond to the nucleotide sequences or 
peptide sequences.
}
\seealso{
\code{\link{Collapse}}, \code{\link{CorrectGapsAndNs}} and
\code{\link{Recollapse}}
}
\examples{
filepath<-system.file("extdata","Toy.GapsAndNs.fna", package="QSutils")
reads <- readDNAStringSet(filepath)

lstCollapsed <- Collapse(reads)
DottedAlignment(lstCollapsed$hseqs)
lstCorrected<-CorrectGapsAndNs(lstCollapsed$hseqs[2:length(lstCollapsed$hseqs)],
                lstCollapsed$hseqs[[1]])
lstCorrected<- c(lstCollapsed$hseqs[1],lstCorrected)
lstCorrected                             
lstRecollapsed<-Recollapse(lstCorrected,lstCollapsed$nr)
lstRecollapsed
}