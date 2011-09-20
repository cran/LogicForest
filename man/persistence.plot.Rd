\name{persistence.plot}
\alias{persistence.plot}

\title{Plot Persistence of a Variable of Interaction
}

\description{Plot the persistence of a particular variable or interaction term in a logic forest.
}

\usage{
persistence.plot(fit, preds, PI, title)
}

\arguments{
  \item{fit}{ an object of class \code{logforest}.
}
  \item{preds}{ a numeric value representing the number of predictors in the original data used to construct the logic forest model.
}
  \item{PI}{ a character string representing the variable or interaction term to be plotted.
}
  \item{title}{ an optional title for the plot
}
}

\details{Persistence plots can be generated for any variable or interaction term that occurs in a logic forest or LBoost model.  Plots can also be generated for variables and interaction terms that do not occur in the model exactly as given in the argument \code{PI} assuming that at least one subset match for that variable/interaction exists.
}

\value{Invisibly all subset matches for the variable or interaction term defined by the argument \code{PI}.
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{vimp.plot}}, \code{\link{submatch.plot}}
}

\examples{
data(logforest.fit, LBoost.fit)

#Persistence of the predictor X5 in the logic forest model logforest.fit
persistence.plot(fit=logforest.fit, preds=50, PI="X5",
title="logforest Persistence for X5")
#Same plot for the LBoost model LBoost.fit
persistence.plot(fit=LBoost.fit, preds=50, PI="X5",
                 title="LBoost Persistence for X5")

#Persistence of the interaction term (X4 & X5) in the logic forest model 
#logforest.fit
persistence.plot(fit=logforest.fit, preds=50, PI="X4 & X5",
                 title="logforest Persistence for interaction X4 & X5")

#Same plot for the LBoost model LBoost.fit
persistence.plot(fit=LBoost.fit, preds=50, PI="X4 & X5",  
                 title="LBoost persistence for interaction X4 & X5")

}



