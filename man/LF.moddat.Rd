\name{LF.moddat}
\alias{LF.moddat}
\docType{data}
\title{Example Dataset for Logic Forest
}
\description{A data frame containing 50 binary predictors, a true binary response, and a noisy binary response.  
}
\usage{data(LF.moddat)}
\format{
  The first 50 columns represent 50 predictor variables which are independently Bernoulli(0.5).  Column 51 represents the true response and follows the logical relationship

L=(X4 or X11) and X5.

Column 52 represents a noisy version of the response, taking the value of the response, L, with probability(0.85) and value 1-L with probability(0.15).
}

\examples{
data(LF.moddat)
}
\keyword{datasets}
