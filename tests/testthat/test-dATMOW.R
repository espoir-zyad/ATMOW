library(testthat)
library(ATMOW)


test_that("dATMOW returns 0 for negative x", {
  result <- dATMOW(-1, theta = 1, k = 1, lambda = 1)

  # Vérifier que la fonction retourne 0 pour x négatif
  expect_equal(result, 0)
})

test_that("dATMOW returns a positive density for positive x", {
  result <- dATMOW(1, theta = 1, k = 1, lambda = 1)

  # Vérifier que la densité est positive pour x positif
  expect_true(result > 0)

  # Tester la valeur retournée pour des paramètres spécifiques
  expected_result <- (4/pi) * 1 * ((1/1)^(1-1)) * (1/1) * exp(-(1/1)^1) / ((1 - exp(-(1/1)^1))^2 + (1 + (1-1) * (1 - exp(-(1/1)^1)))^2)
  expect_equal(result, expected_result, tolerance = 1e-8)
})

test_that("dATMOW returns correct density for given parameters", {
  x <- 2
  theta <- 0.5
  k <- 2
  lambda <- 1
  result <- dATMOW(x, theta, k, lambda)

  # Calculer la densité attendue manuellement pour les paramètres donnés
  term1 <- (4/pi) * theta * ((x/lambda)^(k-1)) * (k/lambda) * exp(-(x/lambda)^k)
  term2 <- (1 - exp(-(x/lambda)^k))
  term3 <- term2^2
  term4 <- (theta + (1-theta) * term2)^2
  term5 <- term3 + term4
  expected_result <- term1/term5

  # Vérifier que la densité calculée est égale à la densité attendue
  expect_equal(result, expected_result, tolerance = 1e-8)
})
