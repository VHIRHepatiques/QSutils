\name{DottedAlignment}
\alias{DottedAlignment}
\title{
Align haplotypes into a dotted alignment
}
\description{
Given an alignment, it takes the first sequence as reference, and depicts
all equivalences in the alignment as dots, leaving only the variants with 
respect to the reference.
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
Mercedes Guerrero-Murillo and Josep Gregori
}

\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")
strs <- DottedAlignment(lst$hseqs)

# Create a data frame to visualize the result.
data.frame(Hpl=strs,stringsAsFactors=FALSE)
}