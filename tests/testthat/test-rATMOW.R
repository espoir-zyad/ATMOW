library(testthat)
library(ATMOW)

test_that("rATMOW returns error for non-positive n", {
  expect_error(rATMOW(0, theta = 1, k = 1, lambda = 1), "Le nombre de valeurs doit être positif")
  expect_error(rATMOW(-1, theta = 1, k = 1, lambda = 1), "Le nombre de valeurs doit être positif")
})

test_that("rATMOW returns a numeric vector of length n", {
  n <- 10
  result <- rATMOW(n, theta = 1, k = 1, lambda = 1)

  # Vérifier que la sortie est un vecteur numérique
  expect_true(is.numeric(result))

  # Vérifier que la longueur du vecteur est égale à n
  expect_equal(length(result), n)
})

test_that("rATMOW returns values within expected range", {
  n <- 1000
  theta <- 0.5
  k <- 2
  lambda <- 1
  result <- rATMOW(n, theta, k, lambda)

  # Vérifier que toutes les valeurs générées sont non négatives
  expect_true(all(result >= 0))

  # Vérifier que la moyenne des valeurs est raisonnable (approximative)
  # expected_mean <- lambda * gamma(1 + 1/k) # Approximation basée sur la distribution Weibull
  # expect_true(mean(result) > 0.5 * expected_mean && mean(result) < 1.5 * expected_mean)
})

