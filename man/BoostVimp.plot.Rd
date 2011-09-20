\name{BoostVimp.plot}
\alias{BoostVimp.plot}

\title{Variable and Interaction Importance Plots for a LBoost Model
}

\description{Dot chart of variable and/or interaction importance for the variables/interactions with the largest magnitude variable importance scores.
}

\usage{
BoostVimp.plot(fit, num=10, pred, norm=TRUE, titles=TRUE)
}

\arguments{
  \item{fit}{ an object of class LBoost.
}
  \item{num}{ number of variables/interactions to be included on plot. 
}
  \item{pred}{ logical.  If TRUE, a variable importance plot is constructed for individual variables.
}  
  \item{norm}{ logical.  If TRUE, variable/interaction importance scores are normalized such that the largest importance score takes value one and all other values are scaled accordingly.
}
  \item{titles}{ logical.  If FALSE, titles are not included on the plot.
}
}


\value{Plots up to three separate plots of the predictors/interactions with the largest magnitude variable importance score. If \code{pred}=TRUE is specified, one plot will be of the largest magnitude individual variable importance scores.  Note, \code{pred.imp} must also have been specified as TRUE when running \code{\link{LBoost}} to be able to generate this plot.  A plot for each type of interaction importance measure will also be generated if \code{PI.imp}="Both" when running \code{\link{LBoost}}.  If only "Permutation" or "AddRemove" was specified for \code{PI.imp}, one plot will be generated for the interaction importance type specified in \code{\link{LBoost}}.
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf wolfb@musc.edu
}

\seealso{ \code{\link{persistence.plot}}, \code{\link{submatch.plot}}
}


\examples{
data(LBoost.fit)

#Plot of top 10 predictors based on variable importance from the LBoost
#model LBoost.fit
BoostVimp.plot(fit=LBoost.fit, num=10, pred=TRUE, norm=TRUE, titles=TRUE)
}



