
library(testthat)

library(ATMOW)

test_that("estimATMOW returns expected output", {
  data <- 1:20

  # Ouvrir un appareil graphique
  png(filename = tempfile())

  result <- estimATMOW(data, theta_init = 1, k_init = 1, lambda_init = 1, bins = 20, xlim = c(0, max(data) * 1.1), n = 1000)

  # Fermer l'appareil graphique
  dev.off()

  # Test que les paramètres estimés sont retournés sous forme de liste
  expect_true(is.list(result))

  # # Vérifier que les éléments attendus sont présents dans la sortie
  # expect_true("theta" %in% names(result))
  # expect_true("k" %in% names(result))
  # expect_true("lambda" %in% names(result))
  #
  # # Tester que les valeurs des paramètres sont numériques
  # expect_true(is.numeric(result$theta))
  # expect_true(is.numeric(result$k))
  # expect_true(is.numeric(result$lambda))
})

