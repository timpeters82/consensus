\name{fitConsensus}
\alias{fitConsensus}
\title{
  Fit row-linear models to all loci
}
\description{
  The main function of this package. Fits a number of row-linear models from a \code{\link{MultiMeasure}} object, one for each matching row of the data matrices contained within it. Outputs a \code{ConsensusFit} object containing per-platform, per locus consensus values for average, sensitivity and precision. 
}
\usage{
  fitConsensus(multimeas)
}
\arguments{
  \item{multimeas}{
    An object of class \code{\link{MultiMeasure}}. 
  }
}

\details{
  For each locus, a row-linear model (Mandel 1994) is fit of the form \deqn{Z_{ij} = a_{i} + b_{i}(x_{j} - \bar{x}) + d_{ij}} where \eqn{Z_{ij}} is a matrix of measurements at the same genomic locus \eqn{k}, the row index \eqn{i=1,\ldots,p} labels the platform or condition (microarray, library prep method for sequencing assay etc.) used and the column index \eqn{j=1,\ldots,n} labels the biological samples that are interrogated at that locus on each of the \eqn{p} platforms. Hence \eqn{a_{i}} is the intercept (row averages of \eqn{Z_{ij}}), \eqn{b_{i}} the slope of the regression line (sensitivity) and \eqn{d_i = (n-2)^{-1}\Sigma_{j}d_{ij}^{2}} the residual mean square (precision) about the \emph{i}th fitted line, noting that higher \eqn{d_i} corresponds to lower precision. Values of \eqn{a_i}, \eqn{b_i} and \eqn{d_i} can be found in the slots of the \code{ConsensusFit} object.

For \code{\link{MultiMeasure}} objects with 10,000 loci or more, a progress message is printed for every 10,000 loci fitted.

}

\value{
  A \code{ConsensusFit} object with slots containing various parameter values from the row-linear fits. More information can be found in the linked class description. Output from this function can then be passed to various plotting functions for data exploration.
}

\references{

Mandel, J. (1994). Analyzing Interlaboratory Data According to ASTM Standard E691. In \emph{Quality and Statistics: Total Quality Management} (pp. 59-59-12). 100 Barr Harbor Drive, PO Box C700, West Conshohocken, PA 19428-2959: ASTM International.
  
Mandel, J. (2012). The statistical analysis of experimental data. Courier Corporation. Chapter 13: ``The Systematic Evaluation of Measuring Processes''.

Ku, H.H. (1969). Precision Measurement and Calibration. Volume 1. Statistical Concepts and Procedures (No. NBS-SP-300-VOL-1). Issued February 1969. US Department of Commerce. Chapter 3.7: ``The Interlaboratory Evaluation of Testing Methods''. Mandel, J. and Lashof, T.W. p. 170.
  
}
\author{
  Tim Peters <t.peters@garvan.org.au>
}
\examples{
data("TCGA")
tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"),
			data=list(U133A, Huex, Agilent, RNASeq))
fit <- fitConsensus(tcga_mm)
}
