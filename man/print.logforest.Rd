\name{print.logforest}
\alias{print.logforest}
\title{Prints Output for a Logic Forest Model
}

\description{Prints a matrix of the most important predictors and interactions, their variable importance scores, and frequency of occurrence for objects fitted by logforest.

}
\usage{
\method{print}{logforest}(x, \ldots) 
}

\arguments{
  \item{x}{ object of class \code{logforest}.
}
  \item{\dots}{ other options are ignored.
}
}

\value{If x$norm=TRUE, all variable importance scores are normalized such that the largest score take value one and all other scores are scaled accordingly.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{logforest}}
}

\examples{
data(logforest.fit)
print(logforest.fit)
}


