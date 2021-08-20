#' Mettre à jour les données de pêches SD OFB pour la plateforme régionale des données naturalistes
#'
#' La fonction compare une version à jour du ficj=hier de données QG
#'
#' @param fichier_shp_a_jour Caractère. Chemin vers le fichier shapefile contenant les données à jour.
#' @param fichier_csv_precedent Caractère. Chemin vers le fichier csv de la précédente livraison
#'     de données à la plateforme.
#' @param fichier_csv_a_jour Caractère. Chemin vers le fichier csv à jour.
#'
#' @return Le dataframe avec les données au format souhaité (long, nommage et typage des variables).
#' @export
#'
#' @examples
#' \dontrun{
#' maj_donnees_sd_atlas(
#'   fichier_shp_a_jour = "donnees_brutes/atlas.shp",
#'   fichier_csv_precedent = "donnees_traitees/2020_08_16_peches_sd_ofb_atlas.csv",
#'   fichier_csv_a_jour = "donnees_traitees/2021_08_16_peches_sd_ofb_atlas.csv"
#' )
#' }
maj_donnees_sd_atlas <- function(
  fichier_shp_a_jour,
  fichier_csv_precedent,
  fichier_csv_a_jour
  )

{

  df_a_jour <- sf::st_read(dsn = fichier_qgis_a_jour)

  # df_anciennes_donnees <- read
  # -	Opérer une jointure gauche pour ajouter à df_a_jour les UUID existants dans df_anciennes_donnees
  # -	Compléter les UUID manquants sur les nouvelles observations
  # -	Exporter en csv

}

