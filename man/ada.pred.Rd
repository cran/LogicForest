\name{ada.pred}
\alias{ada.pred}
\title{Internal Logic Forest Functions
}
\description{This function is called by \code{\link{PlusMinus.PIimp}}.  It is not intended to be used independently of this function.  
}
\details{Determines the weighted prediction of trees in an LBoost model.  Each prediction is weighted by the tree specific weight that is generated as the LBoost model is constructed.  The predicted values are used to determine the add-in/leave-out measure of interaction importance.
}
\author{Bethany Wolf wolfb@musc.edu
}
\note{This function is supplementary and not intended to be used independently of the other functions.
}
\keyword{internal}
