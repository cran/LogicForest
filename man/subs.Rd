\name{subs}
\alias{subs}
\title{Internal Logic Forest Functions
}
\description{This function is called by \code{persistence.prep}.  It is not intended to be used independently of this function.  
}
\usage{
subs(allPI.list, preds, match.list)
}
\arguments{
  \item{allPI.list}{al list of all interactions identified by a logic forest model.
}
  \item{preds}{a numeric value representing the number of predictors in the original dataset.
}
  \item{match.list}{a vector of all interaction names to be considered for matching.
}
}
\author{Bethany Wolf wolfb@musc.edu
}

\note{This function is supplementary and not intended to be used  independently of the other functions.
}
\keyword{internal}
