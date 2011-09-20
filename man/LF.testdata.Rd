\name{LF.testdata}
\alias{LF.testdata}
\docType{data}
\title{Example of New Test Data for Prediction in Logic Forest and LBoost
}

\usage{LF.testdata}

\description{LF.testdata is simulated case control data where the ratio of cases:controls is 1:1.  The dataset contains 100 observations and 50 binary predictors.  Columns 1,2,....,50 are the binary predictors.  Column 51 is the true binary response and column 52 is a noisy binary response.  Each predictor is simulated as an independent Bernoulli random variable with success probability 0.5.  The response variable is simulated from the model 

L=(X4 and X5) or (X9 and X10)

The true response in column 51 takes value 1 if either of these conditions occurs and takes value 0 otherwise.  The probability that the noisy response (column 52) has the same value as the true response is 90%, designed to simulate 10% error in the data.  Also in these data, 90% of cases match the prime implicant (X4 and X5) and 10% match the prime implicant (X9 & X10).  
}

\examples{
data(LF.testdata)
}
\keyword{datasets}



