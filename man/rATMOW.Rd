\name{rATMOW}
\alias{rATMOW}
\title{Generate a random sample from the Arctan Marshall Olkin Weibull (ATMOW) distribution}

\description{
Generate a random sample from the Arctan Marshall Olkin Weibull (ATMOW) distribution.
}

\usage{
rATMOW(n, theta, k, lambda)
}

\arguments{
  \item{n}{Number of observations in the sample.}
  \item{theta}{First parameter of the distribution.}
  \item{k}{Second parameter of the distribution.}
  \item{lambda}{Third parameter of the distribution.}
}


\value{
  Random sample from the ATMOW distribution.
}

\examples{
# Generate a random sample with 50 observations using estimated parameters
sample <- rATMOW(N = 50, theta = 0.5, k = 2, lambda = 3)
}

