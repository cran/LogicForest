\name{LF.testdat}
\alias{LF.testdat}
\docType{data}
\title{Example Test Dataset for Logic Forest
}
\description{A data frame containing 50 binary predictors and a true binary response.  
}

\usage{data(LF.moddat)}

\format{
  The first 50 columns represent 50 predictor variables which are independently Bernoulli(0.5).  Column 51 represents the true response and follows the logical relationship

L=(X4 or X11) and X5.
}

\examples{
data(LF.moddat)
}
\keyword{datasets}

