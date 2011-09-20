\name{predict.LBoost}
\alias{predict.LBoost}
\title{Prediction of Response Using LBoost
}

\description{Computes predicted values for newdata for as LBoost model fitted using \code{LBoost}.
}

\usage{
\method{predict}{LBoost}(object, newdata, \ldots)
}

\arguments{  
  \item{object}{ an object of class \code{LBoost}.
}
  \item{newdata}{ a matrix or data frame containing new predictor values.  If not given, predictions will be made for the original data used to construct the model.
}
  \item{\ldots}{ not currently used.}
}

\value{
If new data are given, predict returns an object of class \code{"LBoost.prediction"}, that includes a list of elements 
  \item{prediction}{ a numeric vector of predicted responses.}
  \item{weighted.prop}{ a numeric vector of the weighted proportion of trees in the forest that predict a class value of one.}
  
If no new data are given, the function returns the LBoost model prediction for the original data.  
}

\author{Bethany Wolf   wolfb@musc.edu
}


\seealso{\code{\link{LBoost}}
}

\examples{
data(LF.data, LF.testdata, LBoost.fit)


#NOTE- this is the LBoost model prediction for the original data
predict(object=LBoost.fit)

#predicting classes for new test data
predict(object=LBoost.fit, newdata=LF.testdata[,1:50])
}


