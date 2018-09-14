\name{NucleotideDiversity}
\alias{NucleotideDiversity}
\title{
Nucleotide diversity
}
\description{
Computes the mean pairwise genetic distance between sequences in the alignment.
}
\usage{
NucleotideDiversity(dst,w=NULL)
}
\arguments{
    \item{dst}{
A "dist" object or a symmetrical matrix with haplotype pairwise distances
(ie,  the output of DNA.dist).

}   
    \item{w}{
An optional numeric vector with the haplotype counts. When \code{w} is NULL,
the same weight is given to each haplotype, and nucleotide diversity is 
computed at the entity level.
}
}
\value{
A value that corresponds to the nucleotide diversity, either by entity or 
abundance, depending on argument \code{w}.
}
\references{
Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. Epub 2014 Jan 2. 
PubMed PMID: 24389655.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}

\seealso{
\code{\link{DNA.dist}}, \code{\link{ReadAmplSeqs}}
}
\examples{
# Load haplotype alignment with abundances.
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

# Compute the DNA distance matrix. 
dst <- DNA.dist(lst$hseqs,model="K80")

NucleotideDiversity(dst, lst$nr) 
NucleotideDiversity(dst)

}