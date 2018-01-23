\name{MultiMeasure-class}
\alias{MultiMeasure-class}
\docType{class}
\title{Multi-platform genomic measurements across the same samples - class}

\description{
An S4 class that stores normalised matched genomic data from multiple platforms and/or laboratory conditions (e.g. from microarrays, RNA-Seq and other sequencing assays).
}

\section{List Components}{
This class has two slots, \code{names} and \code{data}.
\describe{
  \item{\code{names}:}{ character vector contains the names of each data type (e.g. RNA-Seq, Agilent etc.). Must be the same \code{length} as \code{data}.}
  \item{\code{data}:}{ list of numeric matrices of identical \code{dim}, \code{rownames} and \code{colnames} where each matrix contains the measurements from the platform/condition described in \code{names}. Rows of each matrix correspond to genomic features and columns to samples. Must be the same length as \code{names}.}
}
}

\seealso{
\code{\link{MultiMeasure}} constructs MultiMeasure objects.
}

\section{Methods}{

\code{MultiMeasure} objects have a \code{show} method that describes the dimensions of the data, in the form: \code{MultiMeasure object with i platforms/conditions, j samples and k measured loci}.
}

\author{Tim Peters <t.peters@garvan.org.au>}

\keyword{classes}

