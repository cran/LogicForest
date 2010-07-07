\name{persist.match}
\alias{persist.match}
\title{Internal Logic Forest Functions
}
\description{Internal function called by \code{\link{persistence.prep2}}.  Not intended to be used independt of this function.
}

\usage{
persist.match(fit, pred.nms, preds, match.list)
}
\arguments{
  \item{fit}{ and object of class \code{logforest}.
}
  \item{pred.nms}{ vector of predictor names.
}
  \item{preds}{ a numeric value representing the number of predictors in the data.
}
  \item{match.list}{ an optional list of names of interactions considered for matching.}
}


\author{Bethany Wolf wolfb@musc.edu
}


\keyword{internal}
