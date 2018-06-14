\name{GetQSData}
\alias{GetQSData}
\title{
Read aligned sequences, filters at minimum abundance and sorts the sequences.
}
\description{
Read amplicon aligned sequences with abundance data, filter at a minimum given abundance, and sort by mutations and abundance.
}
\usage{
GetQSData(flnm,min.pct=0.1,type)
}
\arguments{
  \item{flnm}{
Fasta file with haplotypes sequences and their frequencies. The header of each haplotype 
in the fasta file is composed of an ID followed by a vertical bar '|' followed by the read 
counts, and eventually followed by another vertical bar
and additional information (i.e. Hpl.2.0001|15874|25.2)
}
  \item{min.pct}{
Minimum abundance, in \%, to filter the reads. Defaults to 0.1\%.
}
  \item{type}{
Character string specifying the type of the sequences in the fasta file. This must be one of "DNA" or "AA". It is "DNA" by default.
}
}
\details{
The fasta file is loaded and the haplotype abundances, as counts, are taken 
from the header of each sequence. Haplotypes with abundances below 
\code{min.pct} \% are filtered out. 
The haplotypes are then sorted first by decreasing order of the number of 
mutations with respect 
to the dominant haplotype, and second by decreasing order of abundances. As a
result the haplotypes are renamed according to the pattern 
\code{Hpl.n.xxxx}, where \code{n} represents the number of mutations, and
\code{xxxx} the abundance order within mutation number.
}
\value{
Returns a list with three elements.
\item{bseqs}{DNAStringSet or AAStringSet with the haplotype sequences.}
\item{nr}{Vector of haplotype counts.}
\item{nm}{Vector of number of mutations of each haplotype with respect to the dominant (most frequent) haplotype.}
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
\code{\link{ReadAmplSeqs}}
}
\examples{
\dontrun{

lst<-GetData("file.fasta",min.pct=0.1,type="DNA")
lst
}}

\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line