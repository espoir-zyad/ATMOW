library(testthat)
library(ATMOW)

test_that("qATMOW returns error for probability outside [0, 1]", {
  expect_error(qATMOW(-0.1, theta = 1, k = 1, lambda = 1), "La probabilité doit être entre 0 et 1")
  expect_error(qATMOW(1.1, theta = 1, k = 1, lambda = 1), "La probabilité doit être entre 0 et 1")
})

test_that("qATMOW returns a numeric value for valid probabilities", {
  p <- 0.5
  theta <- 1
  k <- 1
  lambda <- 1
  result <- qATMOW(p, theta, k, lambda)

  # Vérifier que la sortie est une valeur numérique
  expect_true(is.numeric(result))
})

test_that("qATMOW returns correct quantile for given parameters", {
  p <- 0.5
  theta <- 0.5
  k <- 2
  lambda <- 1
  result <- qATMOW(p, theta, k, lambda)

  # Calculer le quantile attendu manuellement pour les paramètres donnés
  expected_result <- lambda * (-log(1 - (theta * tan(p * pi/4 )) / (1 - (1 - theta) * tan(p * pi/4))))^(1/k)

  # Vérifier que le quantile calculé est égal au quantile attendu
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("qATMOW returns 0 for probability 0", {
  result <- qATMOW(0, theta = 1, k = 1, lambda = 1)

  # Vérifier que la fonction retourne 0 pour une probabilité de 0
  expect_equal(result, 0)
})


