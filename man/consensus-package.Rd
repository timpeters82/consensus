\name{consensus-package}
\alias{consensus-package}
\alias{consensus}
\docType{package}
\title{
Cross-platform consensus analysis of genomic measurements via interlaboratory testing method
}
\description{
An implementation of the American Society for Testing and
Materials (ASTM) Standard E691 for interlaboratory testing 
procedures, designed for cross-platform genomic measurements. 
Given three (3) or more genomic platforms or laboratory 
protocols, this package provides interlaboratory testing 
procedures giving per-locus comparisons for sensitivity 
and precision between platforms.
}
\author{
Tim J. Peters <t.peters@garvan.org.au>
}

\examples{
data("TCGA")
tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
			data=list(U133A, Huex, Agilent, RNASeq))
fit <- fitConsensus(tcga_mm)
}
