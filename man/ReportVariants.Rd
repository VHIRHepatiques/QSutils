\name{ReportVariants}
\alias{ReportVariants}
\title{
Report variants
}
\description{
Reports the variants of a DNAStringSet or AAStringSet of haplotypes 
given a reference sequence.
}
\usage{
ReportVariants(hseqs,ref.seq,nr=NULL,start=1)
}
\arguments{
    \item{hseqs}{
DNAStringSet or AAstringSet object of the aligned haplotypes.
}
    \item{ref.seq}{
Character vector with the reference sequence of the alignment.
}
    \item{nr}{
Numeric vector with the abundances of each haplotype in hseqs. When \code{nr}
is NULL, a vector of ones is taken as default.
}
    \item{start}{
Position of the first nucleotide in the alignment
}
}

\value{
A dataframe with 4 columns: the nucleotide in the reference sequence, 
the position, the variant nucleotide, and its abundance.
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
\examples{

# Load objects.
lst<-ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

# Report the variants in these haplotypes,
# taking as a reference the most abundant haplotype.
ReportVariants(lst$hseqs[-1], ref.seq= as.character(lst$hseqs[1]), 
lst$nr[-1], start = 1)
}