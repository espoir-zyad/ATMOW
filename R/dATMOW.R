#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



#' Densité de la distribution Arctan Marshall Olkin Weibull
#'
#' @param x Valeur pour laquelle calculer la densité
#' @param theta Premier paramètre de la distribution provenant de la famille Arctan X
#' @param k Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @param lambda Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @return La densité de probabilité en x
#' @export


dATMOW <- function(x, theta, k, lambda) {
  if (x < 0) {

    return(0)

  } else {

    term1 <- (4/pi) * theta * ((x/lambda)^(k-1)) * (k/lambda) * exp(-(x/lambda)^k)
    term2 <- (1 - exp(-(x/lambda)^k))
    term3 <- term2^2
    term4 <- (theta + (1-theta) * term2)^2
    term5 <- term3 + term4
    density <- term1/term5

    return(density)

  }
}
