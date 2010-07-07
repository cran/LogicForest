\name{logforest}
\alias{logforest}

\title{Logic Forest
}

\description{Constructs an ensemble of logic regression models for classification and identification of important predictors and predictor interactions
}

\usage{
logforest(resp, Xs, nBSXVars, anneal.params, nBS = 100, h = 0.5, 
norm = TRUE, numout = 5)
}

\arguments{
  \item{resp}{numeric vector of binary response values.
}
  \item{Xs}{matrix or dataframe of zeros and ones for all predictor variables.
}
  \item{nBSXVars}{integer for the number of predictors used to construct each logic regression model.  The default value is all predictors in the data.
}
  \item{anneal.params}{a list containing the parameters for simmulated annealing.  See the help file for the function \code{logreg.anneal.control} in the \code{LogicReg} package.  If missing, default annealing parameters are set at \code{start}=1, \code{end}=-2, and \code{iter}=50000.
}
  \item{nBS}{number of logic regression trees to be fit in the logic forest model.
}
  \item{h}{a number bewteen 0 and 1 for the minimum propotion of trees in the logic forest that must predict a 1 for the prediction to be one.
}
  \item{norm}{logical.  If FALSE, predictor and interaction scores in model output are not normalized to range between zero and one.
}
  \item{numout}{number of predictors and interactions to be included in model output
}
}

\details{
}

\value{An object of class \code{"logforest"} which is a list including values
  \item{AllFits }{A list of all logic regression fits in the logic forest model.}
  \item{Top5.PI }{a vector of the 5 interactions with the largest magnitude variable importance score.}
   \item{Predictor.importance }{a vector of importance scores for all predictors that occur in the logic forest.}
   \item{PI.importance }{a vector of importance scores for all interactions that occur in the logic forest.}
  \item{Predictor.frequency }{a vector frequency of predictors occuring in individual logic regression in the logic forest.}
  \item{PI.frequency }{a vector frequency of interactions occuring in individual logic regression in the logic forest.}
  \item{ModelPI.import }{a list on interaction importance measures for each logic regression model in the logic forest.}
  \item{OOBmisclass }{out-of-bag error estimate for the logic forest.}
  \item{OOBprediction}{a matrix.  Column one is the out-of-bag prediction for responses in original data.  Columns 2 is the proporiton of out-of-bag trees that predicted class value to be one.}
  \item{IBdata }{a list of all in-bag data sets for the logic forest model.}
  \item{OOBdata }{a list of all out-of-bag data sets for the logic forest model.}
  \item{norm }{logical.  If TRUE the normalized predictor and interaction importance scores are returned.}
  \item{numout }{the number of predictors and interactions (based on the variable importance measure) to be returned by logforest.}
  \item{predictors }{number of predictor variables in the data used to construct the logic forest.}
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf wolfb@musc.edu
}

\seealso{\code{\link{print.logforest}}, \code{\link{predict.logforest}}, \code{\link{vimp.plot}}, \code{\link{submatch.plot}}, \code{\link{persistence.plot}}
}

\examples{
data(LF.moddat)

#Set using annealing parameters using the logreg.anneal.control 
#function from LogicReg package
newanneal<-logreg.anneal.control(start=1, end=-2, iter=2500)

fit1<-logforest(resp=LF.moddat$Ybin, Xs=LF.moddat[,1:50], nBS=20,
 anneal.params=newanneal)
print(fit1)
predict(fit1)

#typically more than 2500 iterations (>25000) would be used for 
#the annealing algorithm.  A typical forest also contains at 
#least 100 trees.  These parameters were set to allow the examples 
#to run fast

#Changing print parameters
fit2<-logforest(resp=LF.moddat$Ybin, Xs=LF.moddat[,1:50], nBS=20,
 anneal.params=newanneal, norm=TRUE, numout=10)
print(fit2)
}

