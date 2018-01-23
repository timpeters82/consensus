\name{plotMostDiscordant}
\alias{plotMostDiscordant}
\title{
Plot a heatmap showing a selection of loci
}
\description{
Plots a heatmap of a specified number of loci showing per-platform, values for either (a) average (\eqn{a_i}), (b) sensitivity (\eqn{b_i}) or (c) precision (\eqn{d_i}) for the most discordant for each. Discordance is ranked by \eqn{V(a_i)}, \eqn{V(b_i)} or \eqn{\frac{\Sigma(d_i)}{p-1}} where \eqn{p=} the number of platforms/conditions. 
}
\usage{
plotMostDiscordant(consfit, param=c("average", "sensitivity", "precision"), 
                   numloci=20, pal=colorRampPalette(brewer.pal(9, "RdYlGn")))
}

\arguments{
  \item{consfit}{An object of class \code{ConsensusFit}.}
  \item{param}{Whether average (\eqn{a_i}), sensitivity (\eqn{b_i}) or precision (\eqn{d_i}) is plotted.}
  \item{numloci}{The number of loci to plot.}
  \item{pal}{Colour palette. Length must be at least the number of platforms/conditions.}
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
fit <- fitConsensus(tcga_mm)
plotMostDiscordant(fit, "sensitivity", 25)
}
