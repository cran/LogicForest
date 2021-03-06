\name{print.LFprediction}
\alias{print.LFprediction}

\title{Prints Logic Forest Prediction Output 
}

\description{Prints predictions from a logic forest model.
}

\usage{
\method{print}{LFprediction}(x, \ldots) 
}

\arguments{
  \item{x}{ an object of class \code{LF.prediction}.
}
  \item{\dots}{ other options are ignored.
}
}

\value{Returns a numeric vector of predicted classes for all observations in \code{newdata} or the out-of-bag predictions for data used to construct the logic forest and a numeric vector of the proportion of trees in a logic forest that predict a class of one.
}

\author{Bethany Wolf   wolfb@musc.edu
}


\seealso{\code{\link{predict.logforest}}
}

\examples{
data(logforest.fit, LF.testdata)

#OOB prediction on original data
predict(logforest.fit)

#Prediction for new test data
predict(logforest.fit, newdata=LF.testdata[,1:50])
}
