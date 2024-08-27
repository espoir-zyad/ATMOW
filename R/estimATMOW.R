#' Estimer les paramètres et tracer l'histogramme et la courbe de la distribution ContDist
#'
#' @param data Vecteur de données pour lequel estimer les paramètres
#' @param theta_init Estimation initiale pour le paramètre theta
#' @param k_init Estimation initiale pour le paramètre k
#' @param lambda_init Estimation initiale pour le paramètre lambda
#' @param bins Nombre de bins pour l'histogramme
#' @param xlim Limites de l'axe x pour le tracé
#' @param n Nombre de points pour le tracé de la courbe de la distribution
#' @return Une liste contenant les paramètres estimés et le tracé
#' @export
estimATMOW <- function(data, theta_init = 1, k_init = 1, lambda_init = 1, bins = 20, xlim = c(0, max(data) * 1.1), n = 1000) {
  if (!is.vector(data)) stop("Les données doivent être un vecteur")
  if (any(data < 0)) stop("Les données doivent être non négatives")

  # Estimation des paramètres
  library(MASS)
  # Fonction de densité ATMO-W
  f_ATMOW <- function(x, params) {

    theta <- params[1]
    k <- params[2]
    lambda <- params[3]

    term1 <- (4/pi) * theta * ((x/lambda)^(k-1)) * (k/lambda) * exp(-(x/lambda)^k)
    term2 <- (1 - exp(-(x/lambda)^k))
    term3 <- term2^2
    term4 <- (theta + (1-theta) * term2)^2
    term5 <- term3 + term4
    density <- term1/term5
    return(density)
  }

  # Fonction de log-vraisemblance négative
  negloglik <- function(params, data) {
    theta <- params[1]
    k <- params[2]
    lambda <- params[3]

    loglik <- sum(-log(f_ATMOW(data, params)))

    return(loglik)
  }

  # Valeurs initiales des paramètres
  start_params <- c(theta_init, k_init, lambda_init)


  lower <- c(0.001, 0.001, 0.001)
  upper <- c(Inf, Inf, Inf)


  fit <- nlminb(start_params, negloglik, data = data, lower = lower, upper = upper)

  # Assurez-vous qu'un nouvel appareil graphique interactif est ouvert
  plot.new()

  # Tracé
  hist(data, breaks = bins, probability = TRUE, xlim = xlim, col = "white", border = "black",
       main = "Histogramme des données avec densité estimées", xlab = "Valeur", ylab = "Densité")

  x <- seq(xlim[1], xlim[2], length.out = n)
  y <- f_ATMOW(x, fit$par)
  lines(x, y, col = "red", lwd = 2)

  plot_object <- recordPlot()  # Enregistre le graphique

  dev.off()  # Ferme l'appareil graphique

  return(list( params = fit$par, plot = plot_object))

}
