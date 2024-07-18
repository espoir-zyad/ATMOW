#' Génération de valeurs aléatoires de la distribution Arctan Marshall Olkin Weibull
#'
#' @param n Nombre de valeurs à générer
#' @param theta Premier paramètre de la distribution provenant de la famille Arctan X
#' @param k Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @param lambda Deuxième paramètre de la distribution provenant de distribution de Weibull
#' @return Un vecteur de n valeurs générées
#' @export
rATMOW <- function(n, theta, k, lambda) {

  if (n <= 0) {

    stop("Le nombre de valeurs doit être positif")

  }else {

    u <- runif(n)
    x <- lambda * (-log(1 - (theta * tan(u * pi/4 )) / (1 - (1 - theta) * tan(u * pi/4))))^(1/k)
    return(x)

  }
}
