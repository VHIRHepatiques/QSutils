\name{DSFT}
\alias{DSFT}
\title{
Donsampling followed by fringe trimming.
}
\description{
Diversity indices are more or less influenced by the sample size on  
which they were computed. This function helps in minimizing the bias 
tied to a sample size. First the vector of abundances is scaled to a 
lower sample size, then all haplotypes with abundances below a given 
threshold are excluded with high confidence.
}
\usage{
DSFT(nr, size, p.cut = 0.002, conf = 0.95)
}
\arguments{
    \item{nr}{
Vector of observed haplotype counts.
}
    \item{size}{
Size to downsample.
}
    \item{p.cut}{
Abundance threshold.
}
    \item{conf}{
Confidence in trimming.
}
}
\value{
Vector of logicals, with false the haplotypes to be excluded.
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
Josep Gregori and Mercedes Guerrero
}

\examples{
\dontrun{
#Algo
}}