\name{NucleotideDiversity}
\alias{NucleotideDiversity}
\title{
Nucleotide diversity.
}
\description{
Computes the mean pairwise genetic distance among sequences in the alignemnt.
}
\usage{
NucleotideDiversity(dst,w=NULL)
}
\arguments{
  \item{dst}{
A "dist" object or a symmetric matrix with haplotype pairwise distances 
(i.e the output of DNA.dist).
}
  \item{w}{
An optional numeric vector with the haplotype counts to be used to take into account the haplotype frequencies. When \code{w} is NULL the same weight is given to each haplotype, and the nucleotide diversity at the entity level is computed.
}
}
\value{
A value that corresponds to the nucleotide diversity, either by entity or abundance depending on argument \code{w}.
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
\code{\link{DNA.dist}}, \code{\link{ReadAmplSeqs}}
}
\examples{
\dontrun{
# Load haplotypes alignment with abundances.
lst <- ReadAmplSeqs("file.fasta")

# Compute the DNA distances matrix.
dst <- DNA.dist(lst$hseqs,model="K80")

NucleotideDiversity(dst, lst$nr)
NucleotideDiversity(dst)
}}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line