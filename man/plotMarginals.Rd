\name{plotMarginals}
\alias{plotMarginals}
\title{
Density plots of per-platform marginal distributions
}
\description{
Plots a series of marginal densities for each platform for either (a) average, (b) sensitivity or (c) precision. 
}
\usage{
plotMarginals(consfit, 
	      param=c("average", "sensitivity", "precision"), 
	      pal=palette())
}

\arguments{
  \item{consfit}{An object of class \code{ConsensusFit}.}
  \item{param}{Whether average (\eqn{a_i}), sensitivity (\eqn{b_i}) or precision (\eqn{d_i}) is plotted.}
  \item{pal}{Colour palette. Length must be at least the number of platforms/conditions.}
}

\details{
Precision is plotted on the log scale.
}
\value{
A plot to the current device.
}

\author{
Tim Peters  <t.peters@garvan.org.au>
}
\examples{
data("TCGA")
tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNASeq"), 
			data=list(U133A, Huex, Agilent, RNASeq))
fit <- fitConsensus(tcga_mm)
plotMarginals(fit, "sensitivity", brewer.pal(n = 4, name = "Dark2"))
}
