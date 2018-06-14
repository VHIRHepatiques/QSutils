\name{ICposition}
\alias{ICposition}
\title{
Plot the information content. 
}
\description{
Display a plot with the information content (IC) at each position in the alignment.
}
\usage{
ICposition(seqs,nr=NULL)
}
\arguments{
  \item{seqs}{
DNAStringSet or AAStringSet object with the sequences of the haplotypes.
}
  \item{nr}{
An optional numeric vector with the haplotype counts to be used to take into account the haplotype frequencies.
}
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. PubMed 
PMID: 24389655.
}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
\dontrun{

# Create the object.
lst <- ReadAmplSeqs("file.fasta")

# Plot of the IC. 
ICposition(lst$hseqs)
# Plot of the IC with haplotype abundances.
ICposition(lst$hseqs,lst$nr)
}}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
