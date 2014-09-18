\name{CV.data}
\alias{CV.data}
\title{Internal Logic Forest Functions
}
\description{This function is called by \code{\link{LBoost}}.  It is not intended to be used independently of this function.  
}
\details{Splits the data into kfold test and training data sets.  The LBoost model is constructed using the training data sets and model error rate is estimated using the associated test data sets.
}
\author{Bethany Wolf wolfb@musc.edu
}
\note{This function is supplementary and not intended to be used independently of the other functions.
}
\keyword{internal}

