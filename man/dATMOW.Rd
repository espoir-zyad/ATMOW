\name{dATMOW}
\alias{dATMOW}
\title{Probability Density Function (PDF) for the Arctan Marshall Olkin Weibull (ATMOW) distribution}

\description{
Calculate the probability density of the Arctan Marshall Olkin Weibull distribution.
}

\arguments{
  \item{x}{Value for which density is calculated.}
  \item{theta}{First parameter of the distribution.}
  \item{k}{Second parameter of the distribution.}
  \item{lambda}{Third parameter of the distribution.}
}

\usage{
dATMOW(x, theta, k, lambda)
}

\details{

The probability density function (PDF) for the ATMOW distribution is defined as follows:

\deqn{
    f(x) = \frac{4}{\pi}\frac{\theta\cdot\frac{k}{\lambda}\cdot\left(\frac{x}{\lambda}\right)^{k-1}\cdot e^{-\left(\frac{x}{\lambda}\right)^{k}}}{\left(1 - e^{-\left(\frac{x}{\lambda}\right)^{k}}\right)^2 + \left[\theta + (1-\theta)\left(1 - e^{-\left(\frac{x}{\lambda}\right)^{k}}\right)\right]^2}
}

}

\value{
  The probability density at x.
}

\examples{
dATMOW(x = 1, theta = 0.5, k = 2, lambda = 3)
}

