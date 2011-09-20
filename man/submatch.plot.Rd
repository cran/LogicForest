\name{submatch.plot}
\alias{submatch.plot}
\title{Plot of Variable/Interaction Frequency
}

\description{Plot of frequency of occurrence of variables/interactions in increasingly larger interactions identified in a logic forest model. 

}

\usage{
submatch.plot(fit, pred.nms, pis, preds, size, color = FALSE)
}
\arguments{
  \item{fit}{ an object of class \code{logforest}.
}
  \item{pred.nms}{ an optional vector of predictor names considered for subset matching.  If missing, all variable names from the original data used to construct the logic forest model are considered.
}
  \item{pis}{ a numeric value representing the number of variables/interactions to be included on the plot.  Variables/interactions are sorted according to frequency of occurrence as subset matches.
}
  \item{preds}{ a numeric value representing the number of predictors in the original dataset.
}
  \item{size}{ a numeric value for the size of the interaction terms represented on the plot.
}
  \item{color}{ logical.  If TRUE, plots are generated using color, otherwise they are in grayscale.
}
}

\details{The center plot shows the most frequently occurring interactions of the size specified in the arguments.  Circles on the plot represent the relative frequency of occurrence on the interactions on the y-axis in interactions of the same size and all larger sizes for those interactions identified in a logic forest model.  The histogram at the top of the plot represents the total number of interaction terms of the size indicated on the x-axis identified by the logic forest model.  The dark gray in this histogram represents the number of interactions of that size that are accounted for by the interactions shown of the y-axis.  The histogram on the right side of the plot represents the frequency of occurrence of all interactions identified in a logic forest model of the size specified in the arguments.  Dark gray bars in the histogram represent the frequency of occurrence of those interactions on the y-axis.
}

\value{Invisibly the number of subset matches at each size for those interactions given in the plot.
}

\references{Wolf, B.J., Slate, E.H., Hill, E.G. (2010) Logic Forest: An ensemble classifier for discovering logical combinations of binary markers.  Bioinformatics.
}

\author{Bethany Wolf   wolfb@musc.edu
}

\seealso{\code{\link{vimp.plot}}, \code{link{persistence.plot}}
}

\examples{
data(logforest.fit, LBoost.fit)

#Plot of subset matches for the 5 most frequently occurring interactions 
#of size 2 in the logic forest model logforest.fit
submatch.plot(fit=logforest.fit, pis=5, preds=50, size=2, color=FALSE)

#Same plot for the LBoost model LBoost.fit
submatch.plot(fit=LBoost.fit, pis=5, preds=50, size=2, color=FALSE)

}



