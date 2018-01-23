\name{plotOneFit}
\alias{plotOneFit}
\title{
Plot a single row-linear fit from a genomic locus
}
\description{
Plots a series of regressions of platform measurements against their consensus mean.
}
\usage{
plotOneFit(multimeas, idx, pal=palette())
}

\arguments{
  \item{multimeas}{An object of class \code{MultiMeasure.}}
  \item{idx}{Row index of the set of matrices in \code{multimeas}.}
  \item{pal}{Color palette. Length must be at least the length of multimeas@data.}
}

\details{
Visualises a row-linear fit explicitly in the measurement space. Steeper (positive) slopes mean greater sensitivity, and greater scatter around the regression line indicates lower precision.
}
\value{
A plot to the current device.
}

\author{
Tim Peters <t.peters@garvan.org.au>
}
\examples{
data("TCGA")
tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNASeq"), 
			data=list(U133A, Huex, Agilent, RNASeq))
plotOneFit(tcga_mm, "TP53", brewer.pal(n = 4, name = "Dark2"))
}
