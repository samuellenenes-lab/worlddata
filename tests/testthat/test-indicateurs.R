test_that("calculer_pib_par_hab ajoute la bonne colonne", {
  df <- data.frame(gdp = c(1000, 2000), population = c(10, 20))
  resultat <- calculer_pib_par_hab(df)
  expect_true("gdp_per_capita" %in% names(resultat))
  expect_equal(resultat$gdp_per_capita, c(100, 100))
})

test_that("calculer_pib_par_hab gère les NA", {
  df <- data.frame(gdp = c(1000, NA), population = c(10, 50))
  resultat <- calculer_pib_par_hab(df)
  expect_true(is.na(resultat$gdp_per_capita[2]))
})

test_that("calculer_pib_par_hab échoue si les colonnes manquent", {
  df <- data.frame(pays = "France", hab = 67000000)
  expect_error(calculer_pib_par_hab(df), "gdp")
})



test_that("comparer_pays renvoie exactement 2 lignes", {
  resultat <- comparer_pays(df_test, "France", "Japan")
  expect_equal(nrow(resultat), 2)
})

test_that("comparer_pays renvoie les bonnes colonnes", {
  resultat <- comparer_pays(df_test, "France", "Japan",
                            indicateurs = c("gdp", "population"))
  expect_equal(names(resultat), c("country", "gdp", "population"))
})

test_that("comparer_pays refuse un pays inconnu", {
  expect_error(comparer_pays(df_test, "France", "Narnia"))
})

test_that("comparer_pays refuse un indicateur inexistant", {
  expect_error(comparer_pays(df_test, "France", "Japan",
                             indicateurs = c("gdp", "bonheur")))
})
