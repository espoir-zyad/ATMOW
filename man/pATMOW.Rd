\name{pATMOW}
\alias{pATMOW}
\title{Cumulative Distribution Function (CDF) for the Arctan Marshall Olkin Weibull (ATMOW) distribution}

\description{
Calculate the cumulative distribution at a given value using the Arctan Marshall Olkin Weibull distribution.
}

\usage{
pATMOW(x, theta, k, lambda)
}

\arguments{
  \item{x}{Value for which density is calculated.}
  \item{theta}{First parameter of the distribution.}
  \item{k}{Second parameter of the distribution.}
  \item{lambda}{Third parameter of the distribution.}
}


\details{

The cumulative distribution function (CDF) for the ATMOW distribution is defined as follows:

\deqn{
    F(x) = \frac{4}{\pi}\arctan\left(\frac{1 - e^{-\left(\frac{x}{\lambda}\right)^{k}}}{\theta + (1-\theta)\left(1 - e^{-\left(\frac{x}{\lambda}\right)^{k}}\right)}\right)
}

}

\value{
  The cumulative distribution at x.
}

\examples{
pATMOW(x = 1, theta = 0.5, k = 2, lambda = 3)
}

