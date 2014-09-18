\name{vimp.plot}
\alias{vimp.plot}

\title{Variable and Interaction Importance Plots for a Logic Forest Model
}

\description{Dot chart of variable and/or interaction importance for the variables/interactions with the largest magnitude variable importance scores.
}

\usage{
vimp.plot(fit, num=10, type=2, norm=TRUE, titles=TRUE)
}

\arguments{
  \item{fit}{an object of class \code{logforest}.
}
  \item{num}{number of variables/interactions to be included on plot.
}
  \item{type}{type of plot to be constructed: \code{type}=0 generates a plot of the individual variables with the largest importance score, \code{type}=1 generated a plot of the interaction terms with the largest importance score, and \code{type}=2 generates side by side plot of the most important single variable and the most important interactions from a logic forest model.
}  
  \item{norm}{logical.  If TRUE, variable/interaction importance scores are normalized such that the largest importance score takes value one and all other values are scaled accordingly.
}
  \item{titles}{logical.  If FALSE, titles are not included on the plot.
}
}


\value{Invisibly the predictors/interactions with the largest magnitude variable importance score.
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{persistence.plot}}, \code{\link{submatch.plot}} 
}

\examples{
data(logforest.fit)

#Plot of top 10 predictors based on variable importance from logforest
vimp.plot(fit=logforest.fit, type=0, norm=FALSE)

#Plot of top 10 interactions based on variable importance from 
vimp.plot(fit=logforest.fit, num=10, type=1, norm=FALSE)

#Plots of top 10 predictors and interactions based on variable
#importance from logforest
vimp.plot(fit=logforest.fit, num=10, type=2, norm=FALSE)

#Plots of top 10 predictors and interactions based on normalized
#variable importance from logforest
vimp.plot(fit=logforest.fit, num=10, type=2)
}


