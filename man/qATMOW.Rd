\name{qATMOW}
\alias{qATMOW}
\title{Quantile function for the Arctan Marshall Olkin Weibull (ATMOW) distribution}

\description{
Calculate the quantile value for a given probability using the the Arctan Marshall Olkin Weibull (ATMOW) distribution.
}

\usage{
qATMOW(p, theta, k, lambda)
}

\arguments{
  \item{p}{Probability for which to calculate the quantile.}
  \item{theta}{First parameter of the distribution.}
  \item{k}{Second parameter of the distribution.}
  \item{lambda}{Third parameter of the distribution.}
}


\value{
  Numeric value representing the quantile.
}

\examples{
qATMOW(p = 0.75, theta = 0.5, k = 2, lambda = 3)
}

