\name{ConsensusFit-class}
\alias{ConsensusFit-class}
\docType{class}
\title{Row-linear fit from multiple platforms/conditions - class}

\description{
  An S4 class that stores parameter value output from \code{\link{fitConsensus}}.
}

\section{Slots}{
  This class has eight slots, each containing parameters from the row-linear fit:
  \describe{
    \item{\code{a_i}:}{ Platform-wise average (intercepts).}
    \item{\code{b_i}:}{ Platform-wise sensitivity (slopes).}
    \item{\code{d_i}:}{ Platform-wise precision (residual mean squares). Note that higher values correspond to lower precision.}
    \item{\code{V_a}:}{ Variance of \code{a_i}. High values indicate high discordance in dynamic range.}
    \item{\code{V_b}:}{ Variance of \code{b_i}. High values indicate high discordance in sensitivity.}
    \item{\code{V_d}:}{ Averaged precision across platforms.}
    \item{\code{z0}:}{ Point of approximate concurrence for all regression lines. Only applicable when \code{a_i} and \code{b_i} are highly correlated. See Equations 13.16 and 13.39 of Mandel (2012).}
    \item{\code{Vdelta}:}{ Residual variance about the line when \code{b_i} is regressed against \code{a_i}. Lower values indicate a higher degree of concurrence, assuming that \code{a_i} and \code{b_i} are highly correlated. See Equation 13.36 of Mandel (2012).}
  }
}

\seealso{
  \code{\link{fitConsensus}}: outputs \code{ConsensusFit} objects.
}

\section{Methods}{
  
  \code{ConsensusFit} objects have a \code{show} method that describes the dimensions of the data, in the form: \code{"ConsensusFit object with i platforms/conditions and k measured loci"}.
}

\references{

Mandel, J. (2012). The statistical analysis of experimental data. Courier Corporation. Chapter 13: \emph{The Systematic Evaluation of Measuring Processes}.

}

\author{ 
Tim Peters <t.peters@garvan.org.au>
}

\keyword{classes}

