\name{SummaryMuts}
\alias{SummaryMuts}
\title{
Distribution of nucleotides or amino acids in polymorphic sites.
}
\description{
Computes the nucleotide or amino acid frequencies in all 
polymorphic sites in the alignment.
}
\usage{
SummaryMuts(seqs, w = NULL, off = 0)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet with the haplotype sequences.
}
    \item{w}{
An optional numeric vector with the haplotype counts to be used to take 
into account the haplotype frequencies. When \code{w} is NULL a vector
of ones is taken as default.
}
    \item{off}{
Offset of first position in the alignment.
}
}
\value{
Data frame with the polymorphic positions and nucleotide or amino 
acid frequencies.
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
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{ReadAmplSeqs}}
}
\examples{

# Load haplotypes alignment with abundances.
lst <- ReadAmplSeqs("QSutils/extdata/ToyData_10_50_1000.fna",type="DNA")

# Distribution of nucleotides in polymorphic sites.
SummaryMuts(lst$hseqs,lst$nr,off=0)
}