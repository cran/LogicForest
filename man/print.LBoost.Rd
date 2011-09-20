\name{print.LBoost}
\alias{print.LBoost}
\title{Prints Output for and LBoost Model
}

\description{Prints a matrix of the most important predictors and interactions, their variable importance scores, and frequency of occurrence for objects fitted by logforest.

}
\usage{
\method{print}{LBoost}(x, num, \ldots) 
}

\arguments{
  \item{x}{ object of class \code{logforest}.
}
  \item{num}{ number of predictors/prime implicants to be included in the print out of the model.  The default value is 5.
}
  \item{\dots}{ other options are ignored.
}
}

\value{If pred.imp was set equal to TRUE when constructing the LBoost model, a matrix including the most important predictors, their normalized importance scores and the frequency of occurrence in the LBoost model will be included.  

If PI.imp was set equal to "Permutation" when constructing the LBoost model, a matrix including the most important predictors, their normalized permutation based importance scores and the frequency of occurrence in the LBoost model will be included.  If PI.imp was set equal to "AddRemove" when constructing the LBoost model, a matrix including the most important predictors, their normalized add-in/leave-out based importance scores and the frequency of occurrence in the LBoost model will be included.  If PI.imp was set equal to "Both" when constructing the LBoost model, two matrices including the most important predictors, their normalized importance scores (by each method) and the frequency of occurrence in the LBoost model will be included.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{LBoost}}
}

\examples{
data(LBoost.fit)
print(LBoost.fit)
}



