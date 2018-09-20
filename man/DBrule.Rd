\name{DBrule}
\alias{DBrule}
\title{
Genotyping by the DB rule
}
\description{
Computes the nearest cluster to a given sequence.
}
\usage{
DBrule(grpDist, hr, oDist, g.names = NULL)
}
\arguments{
    \item{grpDist}{
Distances between reference sequences.
}
    \item{hr}{
Factor or a vector of integers that contains the type or subtype for each 
reference sequence.
}
    \item{oDist}{
Distance from the sequence to be classified to the reference sequences.
}
    \item{g.names}{
Type or subtype names to classify the sequence.
}
}
\value{
List with three elements:
    \item{Phi2}{Vector with the distances to each cluster.}
    \item{DB.rule}{The index of the nearest cluster.}
    \item{Type}{Name of the nearest cluster.}
}
\references{
Caballero A, Gregori J, Homs M, Tabernero D, Gonzalez C, Quer J, Blasi M,
Casillas R, Nieto L, Riveiro-Barciela M, Esteban R, Buti M, Rodriguez-Frias F.
Complex Genotype Mixtures Analyzed by Deep Sequencing in Two Different 
Regions of Hepatitis B Virus. PLoS One. 2015 Dec 29;10(12):e0144816. doi:
10.1371/journal.pone.0144816. eCollection 2015. PubMed PMID: 26714168; PubMed
Central PMCID: PMC4695080.
}
\author{
Mercedes Guerrero-Murillo and Josep Gregori
}

\examples{

# Load haplotype to be genotyped.
filepath<-system.file("extdata","Unknown-Genotype.fna", package="QSutils")
lst <- ReadAmplSeqs(filepath,type="DNA")
hseq <- lst$hseq[1]

# Load genotype references.
filepath_geno<-system.file("extdata","GenotypeStandards_A-H.fas", package="QSutils")
RefSeqs <- readDNAStringSet(filepath_geno)

# Compute pairwise distances.
dm <- as.matrix(DNA.dist(c(hseq,RefSeqs),model="K80"))

# Distances between genotype RefSeqs
dgrp <- dm[-1,-1]
grp <- factor(substr(rownames(dgrp),1,1))
hr <- as.integer(grp)

# Distance of the query haplotype to the reference sequence.
d <- dm[1,-1]

# Genotyping by the DB rule.
dsc <- DBrule(dgrp,hr,d,levels(grp))
dsc
}

