#' Fonction de répartition de la distribution Arctan Marshall Olkin Weibull
#'
#' @param x Valeur pour laquelle calculer la probabilité cumulative
#' @param theta Premier paramètre de la distribution provenant de la famille Arctan X
#' @param k Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @param lambda Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @return La probabilité cumulative jusqu'à q
#' @export
pATMOW <- function(q, theta, k, lambda) {
  if (q < 0) {

    return(0)

  } else {

    term1 <- (1 - exp(-(q/lambda)^k))
    F_ATMOW <- (4/pi) * atan(term1/(theta + (1-theta)*term1))
    return(F_ATMOW)

  }
}
