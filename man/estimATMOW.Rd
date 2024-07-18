\name{estimATMOW}
\alias{estimATMOW}
\title{Parameters estimation using the Arctan Marshall Olkin Weibull (ATMOW) distribution}

\description{

     This function takes as parameters a dataset, initial values of the parameters of the ATMOW distribution as well as parameters such as bins, xlim and n and returns the estimated values of the parameters theta, k and lambda as well as the histogram of the data followed by the curve of the estimated parameters of the ATMOW distribution.

}

\usage{
estimATMOW(data, theta_init = 1, k_init = 1, lambda_init = 1, bins = 20, xlim = c(0, max(data) * 1.1), n = 1000)
}

\arguments{
  \item{data}{Data vector for which to estimate parameters.}
  \item{theta_init}{Initial estimate for theta parameter.}
  \item{k_init}{Initial estimate for parameter k.}
  \item{lambda_init}{Initial estimate for parameter lambda.}
  \item{bins}{Number of bins for histogram.}
  \item{xlim}{x-axis limits for plotting.}
  \item{n}{Number of points for plotting the distribution curve.}
}


\value{
  The estimated parameters of the ATMOW distribution and the histogram of the data, followed by a graphical representation of the distribution with estimated parameters.
}

\examples{
data <- 1:20
result <- estimATMOW(data, theta_init = 1, k_init = 1, lambda_init = 1, bins = 20, xlim = c(0, max(data) * 1.1), n = 1000)
}

