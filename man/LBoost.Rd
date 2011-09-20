\name{LBoost}
\alias{LBoost}

\title{LBoost
}

\description{Constructs an ensemble of logic regression models using boosting for classification and identification of important predictors and predictor interactions
}

\usage{
LBoost(resp, Xs, anneal.params, nBS = 100, kfold = 5, nperm = 1, 
PI.imp = NULL, pred.imp = FALSE)
}

\arguments{
  \item{resp}{ numeric vector of binary response values.
}
  \item{Xs}{ matrix or data frame of zeros and ones for all predictor variables.
}
  \item{anneal.params}{a list containing the parameters for simulated annealing.  See the help file for the function \code{logreg.anneal.control} in the \code{LogicReg} package.  If missing, default annealing parameters are set at \code{start}=1, \code{end}=-2, and \code{iter}=50000.
}
  \item{nBS}{ number of logic regression trees to be fit in the LBoost model.
}
  \item{kfold}{ The number of times the data are to be split in constructing the ensemble.
}
  \item{nperm}{ If measuring predictor importance of interaction importance using the permutation based measure, nperm is the number of permutations to be done in determining predictor of interaction importance.
}
  \item{PI.imp}{ A character string describing which measure of interaction importance will be used.  Possible values include "Permutation", "AddRemove", and "Both".  Using "Permutation" will provide the permutation based measure of interaction importance,  "AddRemove" will provide the add-in/leave-out based measure of interaction importance, and "Both" provides both measures of importance.
}
  \item{pred.imp}{ logical.  If FALSE, predictor importance scores will not be measured.
}
}

\details{
}

\value{An object of class \code{"LBoost"} which is a list including values
  \item{CVmod}{ A list of all logic regression fits and the associated information in the LBoost model.  Each item in the list also gives a list of LR fits for a specific kfold data set, a matrix of weights given to each LR fit for that kfold data set, a matrix of the kfold training data used to construct the list of fits.}
  \item{CVmisclass}{ a list including the mean cross-validation misclassification rate for the models and a list of vectors giving the predictions for each of the kfold test data sets.}
   \item{AddRemove.PIimport}{ If PI.imp is specified as either "AddRemove" or "Both, this is a vector of add-in/leave-out importance scores for all interactions that occur in the LBoost model. If PI.imp is not specified or is "Permutation", this will state "Not measured".}
   \item{Perm.PIimport}{ If PI.imp is specified as either "Permutation" or "Both, this is a vector of add-in/leave-out importance scores for all interactions that occur in the LBoost model. If PI.imp is not specified or is "AddRemove", this will state "Not measured".}
   \item{Pred.import}{ If pred.imp is specified as TRUE, a vector of importance scores for all predictors in the data.}
  \item{Pred.freq }{ a vector frequency of predictors occurring in individual logic regression in the LBoost model.}
  \item{PI.frequency }{ a vector frequency of interactions occurring in individual logic regression in the LBoost model.}
  \item{wt.mat}{ a list containing kfold matrices of observation weights for each tree for the kfold training data sets.}
  \item{alphas }{ a list containing kfold vectors of tree specific weights for trees constructed from each of the kfold training data sets.}
  \item{data }{ A matrix of the original data used to construct the LBoost model.}
  \item{PIimp }{ A character string describing which interaction importance measure was used.}
  \item{PredImp }{ logical.  If TRUE predictor importance was measured.}
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{print.LBoost}}, \code{\link{predict.LBoost}}, \code{\link{BoostVimp.plot}}, \code{\link{submatch.plot}}, 
\code{\link{persistence.plot}}
}

\examples{
data(LF.data)

#Set using annealing parameters using the logreg.anneal.control 
#function from LogicReg package
newanneal<-logreg.anneal.control(start=1, end=-2, iter=2500)

#typically more than 2500 iterations (>25000) would be used for 
#the annealing algorithm.  A typical LBoost models also contains at 
#least 100 trees.  These parameters were set to allow for faster
#run time

#The data set LF.data contains 50 binary predictors and a binary response Ybin
#Looking at only the Permutation Measure
LBfit.1<-LBoost(resp=LF.data$Ybin, Xs=LF.data[,1:50], nBS=20, kfold=4,
anneal.params=newanneal, nperm=2, PI.imp="Permutation")
print(LBfit.1)

#Looking at only the Add-in/Leave-out importance measure
LBfit.2<-LBoost(resp=LF.data$Ybin, Xs=LF.data[,1:50], nBS=20, kfold=4,
anneal.params=newanneal, PI.imp="AddRemove")
print(LBfit.2)

#Looking at both measures of importance plus predictor importance
LBfit.3<-LBoost(resp=LF.data$Ybin, Xs=LF.data[,1:50], nBS=20, kfold=4,
anneal.params=newanneal, nperm=2, PI.imp="Both", pred.imp=TRUE)
print(LBfit.3)
}


