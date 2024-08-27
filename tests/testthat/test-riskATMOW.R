library(testthat)
library(ATMOW)

test_that("riskATMOW returns correct VaR and TVaR", {
  # Test with specific values for theta, k, lambda, and q
  theta <- 0.5
  k <- 2
  lambda <- 1
  q <- 0.95

  # Run the riskATMOW function
  results <- riskATMOW(theta = theta, k = k, lambda = lambda, q = q)

  # Expected VaR and TVaR calculations (adjust based on your expectations)
  expected_VaR <- lambda * (-log(1 - (theta * tan(q * pi / 4)) / (1 - (1 - theta) * tan(q * pi / 4))))^(1/k)
  M <- function(x) x * (4/pi) * theta * ((x/lambda)^(k-1)) * (k/lambda) * exp(-(x/lambda)^k) / (((1 - exp(-(x/lambda)^k))^2) + ((theta + (1 - theta) * (1 - exp(-(x/lambda)^k)))^2))
  integral <- tryCatch(
    integrate(M, expected_VaR, Inf),
    error = function(e) NA
  )
  expected_TVaR <- if (!is.na(integral$value)) integral$value / (1 - q) else NA

  # Check that the results are correct
  expect_equal(results$VaR, expected_VaR, tolerance = 1e-6)
  expect_equal(results$TVaR, expected_TVaR, tolerance = 1e-6)
})

test_that("riskATMOW handles invalid inputs", {
  # Test with invalid values for theta, k, lambda, and q
  expect_error(riskATMOW(theta = -0.5, k = 2, lambda = 1, q = 0.95), "parameter theta must be between 0 and 1")
  expect_error(riskATMOW(theta = 0.5, k = -1, lambda = 1, q = 0.95), "parameter k must be positive")
  expect_error(riskATMOW(theta = 0.5, k = 2, lambda = 0, q = 0.95), "parameter lambda must be positive")
  expect_error(riskATMOW(theta = 0.5, k = 2, lambda = 1, q = 1.5), "quantile q must be between 0 and 1")
})
