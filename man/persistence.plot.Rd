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
  \item{PI}{ a charachter string representing the variable or interaction term to be plotted.
}
  \item{title}{ an optional title for the plot
}
}

\details{Persistence plots can be generated for any variable or interaction term that occurs in a logic forest model.  Plots can also be generated for variables and interaction terms that do not occur in the logic forest model exactly as given in the argument PI.
}

\value{Invisibly all subset matches for the variable or interaction term defined by the arguement PI.
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{vimp.plot}}, \code{\link{submatch.plot}}
}

\examples{
data(LFfit1)

#Persistence of the predictor X5 in the logic forest model LFfit1
persistence.plot(fit=LFfit1, preds=50, PI="X5", title="Persistence for
 X5")

#Persistence of predictor X10- not persistent in the model 
persistence.plot(fit=LFfit1, preds=50, PI="!X10", title="Persistence 
for X10")

#Persistence of the interaction term (X5 & X11) in LFfit1
persistence.plot(fit=LFfit1, preds=50, PI="X5 & X11",  
title="Persistence for X5 & X11")

}
