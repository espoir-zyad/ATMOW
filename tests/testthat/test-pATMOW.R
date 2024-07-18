library(testthat)
library(ATMOW)

test_that("pATMOW returns 0 for negative q", {
  result <- pATMOW(-1, theta = 1, k = 1, lambda = 1)

  # Vérifier que la fonction retourne 0 pour q négatif
  expect_equal(result, 0)
})

test_that("pATMOW returns values within [0, 1] for non-negative q", {
  q <- 1
  theta <- 1
  k <- 1
  lambda <- 1
  result <- pATMOW(q, theta, k, lambda)

  # Vérifier que la valeur retournée est dans l'intervalle [0, 1]
  expect_true(result >= 0 && result <= 1)
})

test_that("pATMOW returns correct cumulative probability for given parameters", {
  q <- 2
  theta <- 0.5
  k <- 2
  lambda <- 1
  result <- pATMOW(q, theta, k, lambda)

  # Calculer la probabilité cumulative attendue manuellement pour les paramètres donnés
  term1 <- (1 - exp(-(q/lambda)^k))
  expected_result <- (4/pi) * atan(term1/(theta + (1-theta)*term1))

  # Vérifier que la probabilité cumulative calculée est égale à la probabilité cumulative attendue
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("pATMOW returns 1 for large q", {
  q <- 1e10
  theta <- 1
  k <- 1
  lambda <- 1
  result <- pATMOW(q, theta, k, lambda)

  # Vérifier que la fonction retourne une valeur proche de 1 pour q très grand
  expect_true(result <= 1)
  expect_true(result > 0.99)
})

