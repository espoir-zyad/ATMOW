\name{riskATMOW}
\alias{riskATMOW}
\title{Calculation of VaR and TVaR for the ATMOW Distribution}
\description{
  This function calculates the Value at Risk (VaR) and the Tail Value at Risk (TVaR) for the ATMOW distribution, given the parameters \code{theta}, \code{k}, and \code{lambda}.
}
\usage{
riskATMOW(theta = 1, k = 1, lambda = 1, q = 0.95)
}
\arguments{
  \item{theta}{Initial value for the parameter \code{theta}.}
  \item{k}{Initial value for the parameter \code{k}.}
  \item{lambda}{Initial value for the parameter \code{lambda}.}
  \item{q}{Quantile order for VaR. Must be between 0 and 1.}
}
\value{
  A list containing the following elements:
  \item{VaR}{Value at Risk for the quantile \code{q}.}
  \item{TVaR}{Corresponding Tail Value at Risk.}
}
\examples{
# Calculate VaR and TVaR for the ATMOW distribution with specific parameters
results <- riskATMOW(theta = 0.5, k = 2, lambda = 1, q = 0.95)

# Display the results
print(paste("VaR:", results$VaR))
print(paste("TVaR:", results$TVaR))
}
\author{
Your Name
}
\seealso{
  \code{\link{some_other_function}} % Reference to other similar functions if necessary
}
