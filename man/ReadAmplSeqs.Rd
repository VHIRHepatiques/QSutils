\name{ReadAmplSeqs}
\alias{ReadAmplSeqs}
\title{
Read fasta file with haplotypes and frequencies.
}
\description{
Loads an alignment of haplotypes and their frequencies from a fasta file.
}
\usage{
ReadAmplSeqs(flnm,type)
}
\arguments{
  \item{flnm}{
File name of a fasta file with haplotype sequences and their frequencies. 
The header of each haplotype in the fasta file is composed of an ID followed 
by a vertical bar '|' followed by the read counts, and eventually followed by
another vertical bar and additional information (i.e. Hpl.2.0001|15874|25.2).
}
  \item{type}{
Character string specifying the type of the sequences in the fasta file. This must be one of "DNA" or "AA". It is "DNA" by default.
}
}
\value{
Returns a list with two elements:
\item{nr}{Vector of the haplotype counts. }
\item{hseqs}{DNAStringSet or AAStringSet with the haplotype DNA sequences or the amino acid sequences.}
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
\code{\link{GetQSData}}
}
\examples{
\dontrun{
lst <- ReadAmplSeqs("file.fna",type="DNA")
lst
}
}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line