\name{CorrectGapsAndNs}
\alias{CorrectGapsAndNs}
\title{
Function to correct an alignment with gaps and Ns.
}
\description{
Corrects positions in a DNAStringSet or AAStringSet of aligned haplotypes, replacing gaps or Ns with the nucleotide or amino acid in the corresponding position in the reference sequence.
}
\usage{
CorrectGapsAndNs(hseqs, ref.seq)
}
\arguments{
  \item{hseqs}{
DNAStringSet or AAStringSet object with the alignment to correct.
}
  \item{ref.seq}{
Character vector with the reference squence of the alignment.
}
}
\value{
DNAStringSet or AAStringSet object with the sequences corrected. Duplicated haplotypes may arise as a consequence of this operation. See \code{\link{Recollapse}}.
}
\references{

Gregori J, Esteban JI, Cubero M, Garcia-Cehic D, Perales C, Casillas R,
Alvarez-Tejado M, Rodríguez-Frías F, Guardia J, Domingo E, Quer J. Ultra-deep
pyrosequencing (UDPS) data treatment to study amplicon HCV minor variants. PLoS
One. 2013 Dec 31;8(12):e83361. doi: 10.1371/journal.pone.0083361. eCollection
2013. PubMed PMID: 24391758; PubMed Central PMCID: PMC3877031.

Ramírez C, Gregori J, Buti M, Tabernero D, Camós S, Casillas R, Quer J,
Esteban R, Homs M, Rodriguez-Frías F. A comparative study of ultra-deep
pyrosequencing and cloning to quantitatively analyze the viral quasispecies using
hepatitis B virus infection as a model. Antiviral Res. 2013 May;98(2):273-83.
doi: 10.1016/j.antiviral.2013.03.007. Epub 2013 Mar 20. PubMed PMID: 23523552.

}
\author{
Josep Gregori and Mercedes Guerrero
}
\seealso{
\code{\link{Recollapse}}
}
\examples{
# Create a random reference sequence.
ref.seq <-GetRandomSeq(50)
ref.seq

# Create an aligment with gaps and Ns. 
symb <- c(".","-","N")
nseqs <- 12
p <- c(0.9,0.06,0.04)
hseqs <- matrix(sample(symb,50*nseqs,replace=TRUE,prob=p),ncol=50)
hseqs <- apply(hseqs,1,paste,collapse="")
hseqs
hseqs <- DNAStringSet(hseqs)

# Appy the function and visualize the result.
cseqs <- CorrectGapsAndNs(hseqs,as.character(ref.seq))
c(ref.seq,as.character(cseqs))
}
