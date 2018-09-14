\name{Collapse}
\alias{Collapse} 
\alias{Recollapse}
\title{
Collapse reads into haplotypes and frequencies
}
\description{
Collapse summarizes aligned reads into haplotypes with their frequencies. 
Recollapse is used to update the collapse after some type of manipulation 
may have resulted in duplicate haplotypes.
}
\usage{
Collapse(seqs)
Recollapse(seqs,nr)
}
\arguments{
    \item{seqs}{
DNAStringSet or AAStringSet object with the sequences to collapse.
}
    \item{nr}{
Vector with the haplotype counts.
}
}
\details{
Recollapse is used when haplotypes may become equivalent after some type of 
manipulation. It removes duplicate sequences and updates their frequencies.
}
\value{
Collapse and Recollapse return a list with two elements.
\item{nr}{Vector of the haplotype counts.}
\item{hseqs}{DNAStringSet or AAStringSet with the haplotype sequence.}
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
using hepatitis B virus infection as a model. Antiviral Res. 
2013 May;98(2):273-83. doi: 10.1016/j.antiviral.2013.03.007. Epub 2013 Mar
20. PubMed PMID: 23523552.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}
\examples{
# Load raw reads.
reads <- readDNAStringSet("QSutils/extdata/Toy.GapsAndNs.fna")
# Collapse this reads into haplotypes
lstCollapsed <- Collapse(reads)
lstCorrected<-CorrectGapsAndNs(lstCollapsed$hseqs[2:length(lstCollapsed$hseqs)],
                lstCollapsed$hseqs[[1]])
#Add again the most abundant haplotype.
lstCorrected<- c(lstCollapsed$hseqs[1],lstCorrected)
lstCorrected
# Recollapse the corrected haplotypes
lstRecollapsed<-Recollapse(lstCorrected,lstCollapsed$nr)
lstRecollapsed
}
