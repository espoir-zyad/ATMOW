#' Fonction quantile de la distribution Arctan Marshall Olkin Weibull
#'
#' @param p Probabilité pour laquelle calculer le quantile
#' @param theta Premier paramètre de la distribution provenant de la famille Arctan X
#' @param k Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @param lambda Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @return Le quantile correspondant à p
#' @export
qATMOW <- function(p, theta, k, lambda) {

  if (p < 0 || p > 1) {

    stop("La probabilité doit être entre 0 et 1")

  } else {

    x <- lambda * (-log(1 - (theta * tan(p * pi/4 )) / (1 - (1 - theta) * tan(p * pi/4))))^(1/k)
    return(x)

  }

}
