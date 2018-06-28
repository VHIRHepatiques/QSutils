\name{DottedAlignment}
\alias{DottedAlignment}
\title{
Aligns the haplotypes into a dotted alignment. 
}
\description{
Given an alignment, takes the first sequence as reference, and turns all
equivalences in the alignment as dots, leaving just the variants with respect to
the reference.
}
\usage{
DottedAlignment(hseqs)
}
\arguments{
    \item{hseqs}{
DNAStringSet or AAStringSet with haplotype sequences.
}
}
\value{
A character string vector of the alignment, with dots in the 
conserved positions. 
}
\author{
Josep Gregori and Mercedes Guerrero
}

\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")
strs <- DottedAlignment(lst$hseqs)

#Create a data frame to visualize the result.
data.frame(Hpl=strs,stringsAsFactors=FALSE)
}