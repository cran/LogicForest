\name{predict.logforest}
\alias{predict.logforest}
\title{Prediction of Response Using Logic Forest
}

\description{Computes predicted values for newdata or the out-of-bag prediction for a logic forest model fitted using \code{logforest}.
}

\usage{
\method{predict}{logforest}(object, newdata, cutoff, \ldots)
}

\arguments{
  \item{object}{ an object of class \code{logforest}.
}
  \item{newdata}{ a matrix or data frame containing new predictor values.  If not given, predictions will be made for the original data used to construct the model.
}
  \item{cutoff}{ a number between 0 and 1 for the minimum proportion of trees in the logic forest that must predict a 1 for the prediction to be one.
}
  \item{\ldots}{ not currently used.}
}

\value{
If new data are given, predict returns an object of class \code{"LFprediction"}, that includes a list of elements 
  \item{LFprediction}{ a numeric vector of predicted responses.}
  \item{proportion_one}{ a numeric vector of the proportion of trees in the forest that predict a class value of one.}
  \item{AllTrees}{ a matrix/vector containing the predicted values of each tree in a logic forest for each response, the proportion of trees that predict a one, and the predicted class for each observations.}

If no new data are given, the function returns the out-of-bag prediction for the original data.  The function returns a matrix.  The first column is the out-of-bag prediction for all observations from the original data and the second column is the proportion of out-of-bag trees the predict a one.
}

\author{Bethany Wolf   wolfb@musc.edu
} 


\seealso{\code{\link{logforest}}
}

\examples{
data(LF.data, LF.testdata, logforest.fit)


#predicting classes for the original data
#NOTE- this is the out-of-bag prediction
predict(object=logforest.fit)

#predicting classes for new test data
predict(object=logforest.fit, newdata=LF.testdata[,1:50])
}




