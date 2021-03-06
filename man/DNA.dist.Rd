\name{DNA.dist}
\alias{DNA.dist}
\title{
Matrix of DNA distances given an alignment
}
\description{
Function to compute a matrix of pairwise distances from DNA sequences using a 
model of DNA evolution. It relies on the \code{dist.dna()} function in the
APE package.

}
\usage{
DNA.dist(seqs, model = "raw", gamma = FALSE, pairwise.deletion = FALSE)
}
\arguments{
    \item{seqs}{
DNAStringSet object with the aligned haplotypes.
}
    \item{model}{
Evolutionary model to compute genetic distance by default "raw", but "N", "TS",
"TV", "JC69", "K80", "F81", "K81", "F84", "BH87", "T92","TN93", "GG95", 
"logdet", "paralin", "indel", or "indelblock" can also be used.
}
    \item{gamma}{
Gamma parameter possibly used to apply a correction to the distances or FALSE 
(by default).
}
    \item{pairwise.deletion}{
A logical indicating whether to delete sites with missing data (gaps) in a 
pairwise manner. The default is to delete sites with at least one missing 
datum  in all sequences.
}
}
\value{
Object of class "dist" with pairwise distances. 
}
\references{

Paradis E., Claude J. and Strimmer K., APE: analyses of phylogenetics 
and evolution in R language. Bioinformatics. 2004, 20, 289-290

Gregori J, Perales C, Rodriguez-Frias F, Esteban JI, Quer J, Domingo E. Viral 
quasispecies complexity measures. Virology. 2016 Jun;493:227-37. doi:
10.1016/j.virol.2016.03.017. Epub 2016 Apr 6. Review. PubMed PMID: 27060566.

Gregori J, Salicrú M, Domingo E, Sanchez A, Esteban JI, Rodríguez-Frías F,
Quer J. Inference with viral quasispecies diversity indices: clonal and NGS
approaches. Bioinformatics. 2014 Apr 15;30(8):1104-1111. 
Epub 2014 Jan 2. PubMed PMID: 24389655.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link[ape]{dist.dna}}
}
\examples{
filepath<-system.file("extdata","ToyData_10_50_1000.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")

dst <- DNA.dist(lst$hseqs,model="N")
dst
}