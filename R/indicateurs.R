#' Calculer le PIB par habitant
#'
#' Ajoute une colonne \code{gdp_per_capita} au data.frame.
#'
#' @param donnees Un data.frame avec les colonnes \code{gdp} et
#'   \code{population}.
#' @return Le data.frame enrichi d'une colonne \code{gdp_per_capita}.
#' @export
#' @examples
#' head(calculer_pib_par_hab(world_data))
calculer_pib_par_hab <- function(donnees) {
  if (!all(c("gdp", "population") %in% names(donnees))) {
    stop("Le data.frame doit contenir les colonnes 'gdp' et 'population'.")
  }
  donnees$gdp_per_capita <- donnees$gdp / donnees$population
  donnees
}


#' Comparer deux pays sur une sélection d'indicateurs
#'
#' @param donnees Un data.frame contenant une colonne \code{country}.
#' @param pays_a Nom du premier pays.
#' @param pays_b Nom du second pays.
#' @param indicateurs Vecteur de noms de colonnes à comparer. Par défaut :
#'   PIB, population, espérance de vie, émissions CO2.
#' @return Un data.frame avec les deux pays en lignes et les indicateurs en
#'   colonnes.
#' @export
#' @examples
#' comparer_pays(world_data, "France", "Germany")
comparer_pays <- function(donnees, pays_a, pays_b,
                          indicateurs = c("gdp", "population",
                                          "life_expectancy", "co2_emissions")) {
  if (!pays_a %in% donnees$country) {
    stop("Pays introuvable : ", pays_a)
  }
  if (!pays_b %in% donnees$country) {
    stop("Pays introuvable : ", pays_b)
  }
  manquants <- setdiff(indicateurs, names(donnees))
  if (length(manquants) > 0) {
    stop("Colonnes manquantes : ", paste(manquants, collapse = ", "))
  }
  selection <- donnees[donnees$country %in% c(pays_a, pays_b),
                       c("country", indicateurs)]
  selection
}
