\name{DottedAlignmentPlot}
\alias{DottedAlignmentPlot}
\title{
Display a plot of a dotted alignment.
}
\description{
Creates a plot of the dotted alignment, that have dotts in the conserved positions.
}
\usage{
DottedAlignmentPlot(d.alignment)
}
\arguments{
  \item{d.alignment}{
Output of the function \code{\link{DottedAlignment}}.
}
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{DottedAlignment}}
}
\examples{
\dontrun{

# Create the object.
lst <- ReadAmplSeqs("file.fasta")
d.alignment <- DottedAlignment(lst$hseqs)

# To display the result in a plot.
DottedAlignmentPlot(d.alignment)
}}