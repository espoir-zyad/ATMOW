#' Calcul de la VaR et de la TVaR pour la distribution ATMOW
#'

#' @param theta Valeur initiale pour le paramètre theta
#' @param k Valeur initiale pour le paramètre k
#' @param lambda Valeur initiale pour le paramètre lambda
#' @param q Ordre du quantile (pour la VaR)
#' @return Une liste contenant la VaR pour le quantile q et la TVaR correspondante
#' @export
#'
riskATMOW <- function(theta = 1, k = 1, lambda = 1, q = 0.95) {

  # Input validation
  if (theta < 0 || theta > 1) {
    stop("parameter theta must be between 0 and 1")
  }
  if (k <= 0) {
    stop("parameter k must be positive")
  }
  if (lambda <= 0) {
    stop("parameter lambda must be positive")
  }
  if (q <= 0 || q >= 1) {
    stop("quantile q must be between 0 and 1")
  }

  # Function of density for the ATMOW distribution
  f_ATMOW <- function(x, theta, k, lambda) {
    term1 <- (4/pi) * theta * ((x/lambda)^(k-1)) * (k/lambda) * exp(-(x/lambda)^k)
    term2 <- (1 - exp(-(x/lambda)^k))
    term3 <- term2^2
    term4 <- (theta + (1-theta) * term2)^2
    density <- term1 / (term3 + term4)
    return(density)
  }

  # Calculation of Value at Risk (VaR)
  VaR <- lambda * (-log(1 - (theta * tan(q * pi / 4)) / (1 - (1 - theta) * tan(q * pi / 4))))^(1/k)

  # Calculation of Tail Value at Risk (TVaR)
  M <- function(x) {
    x * f_ATMOW(x, theta, k, lambda)
  }

  # Integrate to find TVaR
  integrale <- tryCatch(
    integrate(M, VaR, Inf),
    error = function(e) NA
  )

  if (is.na(integrale$value)) {
    stop("Error in integration")
  }

  TVaR <- integrale$value / (1 - q)

  return(list(VaR = VaR, TVaR = TVaR))
}
