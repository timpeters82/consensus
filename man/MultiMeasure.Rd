\name{MultiMeasure}
\alias{MultiMeasure}

\title{
MultiMeasure Constructor
}

\description{
Creates a \code{MultiMeasure} object from a set of 3 or more numeric matrices, in preparation to pass to \code{fitConsensus}.
}

\usage{
MultiMeasure(names=NA_character_, data=list())
}

\arguments{
  \item{names}{character vector contains the names of each data type (e.g. RNA-Seq, Agilent etc.). Must be the same \code{length} as \code{data}.}
  \item{data}{list of numeric matrices of identical \code{dim}, \code{rownames} and \code{colnames} where each matrix contains the measurements from the platform/condition described in \code{names}. Rows of each matrix correspond to genomic features and columns to samples. Must be the same length as, and have order correspond to, \code{names}.}
  
}

\details{
A MultiMeasure contains a list of numeric matrices with identical dimensions and matching row names and column names, to which multiple row-linear models can be fit using \code{fitConsensus}. Users should pass a vector of \code{names} describing the platform/conditions the genomic measurements are made under, and a corresponding list of matrices to the \code{data} argument. A series of validity checks will be made on data correctness and a helpful error message will be returned if the structure does not conform to the above description.
}

\value{a \code{\link{MultiMeasure}} object}

\author{Tim Peters <t.peters@garvan.org.au>}

\seealso{\code{MultiMeasure-class}}

\examples{
data(TCGA)
tcga_mm <- MultiMeasure(names=c("U133A", "Huex", "Agilent", "RNA-Seq"), 
			data=list(U133A, Huex, Agilent, RNASeq))
}

