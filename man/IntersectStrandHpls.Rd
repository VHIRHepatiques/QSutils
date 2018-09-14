\name{IntersectStrandHpls}
\alias{IntersectStrandHpls}
\title{
Forward and reverse strand haplotype intersections
}
\description{
Computes the intersection of forward and reverse strand haplotypes after a
previous abundance filter that removes strand haplotypes below a given 
frequency threshold or unique to a single strand.
}
\usage{
IntersectStrandHpls(nrFW, hseqsFW, nrRV, hseqsRV, thr = 0.001)
}
\arguments{
    \item{nrFW}{
Numeric vector with forward strand haplotype counts.
}
    \item{hseqsFW}{
DNAStringSet object with the forward strand haplotypes.
}
    \item{nrRV}{
Numeric vector with forward reverse strand haplotypes.
}
    \item{hseqsRV}{
DNAStringSet object with the reverse strand haplotypes.
}
    \item{thr}{
Threshold to filter haplotypes at minimum abundance.
}
}
\value{
List object with this elements:
    \item{hseqs}{DNAStringSet object with the forward and reverse strand
    intersected.}
    \item{nr}{Numeric vector with the abundance of each haplotype.}
    \item{pFW}{Vector of abundances of aligned forward strand.}
    \item{pRV}{Vector of abundances of aligned reverse strand.}
}
\references{

Gregori J, Esteban JI, Cubero M, Garcia-Cehic D, Perales C, Casillas R,
Alvarez-Tejado M, Rodríguez-Frías F, Guardia J, Domingo E, Quer J. Ultra-deep
pyrosequencing (UDPS) data treatment to study amplicon HCV minor variants. PLoS
One. 2013 Dec 31;8(12):e83361. doi: 10.1371/journal.pone.0083361. eCollection
2013. PubMed PMID: 24391758; PubMed Central PMCID: PMC3877031.

Ramírez C, Gregori J, Buti M, Tabernero D, Camós S, Casillas R, Quer J,
Esteban R, Homs M, Rodriguez-Frías F. A comparative study of ultra-deep
pyrosequencing and cloning to quantitatively analyze the viral quasispecies 
using hepatitis B virus infection as a model. Antiviral Res. 2013 
May;98(2):273-83. doi: 10.1016/j.antiviral.2013.03.007. Epub 2013
Mar 20. PubMed PMID: 23523552.

}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{

# Load objects.
FW<- ReadAmplSeqs("QSutils/extdata/ToyData_FWReads.fna",type="DNA")
RV<- ReadAmplSeqs("QSutils/extdata/ToyData_RVReads.fna",type="DNA")

# Intersect the two objects, with a default threshold.
IntersectStrandHpls(FW$nr,FW$hseqs,RV$nr,RV$hseqs)
}