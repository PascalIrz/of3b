#' Exporter au format csv le tableau de données atlas mis à jour
#'
#' @param df Le dataframe contenant les données mises à jour.
#' @param fichier_csv_sortie Caractère. Chemin vers le fichier csv de sortie. S'il n'est pas précisé,
#'     la fonction va nommer le fichier CSV d'après les date et heure du système et l'écrire
#'     dans un sous-répertoire "processed_data" du répertoire du travail.
#'
#' @return Un fichier csv au format souhaité.
#' @export
#'
#' @importFrom stringr str_replace str_replace_all
#'
#' @examples
#' \dontrun{
#' export_donnees_sd_atlas(
#'   df = data,
#'   fichier_csv_sortie = "donnees_traitees/2020_08_16_peches_sd_ofb_atlas.csv"
#' )
#' }
export_donnees_sd_atlas <- function(df, fichier_csv_sortie = NA)

{
# création du nom et répertoire de sortie s'ils ne sont pas indiqués
if(is.na(fichier_csv_sortie)) {

  if (!dir.exists("processed_data")) {
    dir.create("processed_data")
  }

  fichier_csv_sortie <- paste0("processed_data/", "sd ", Sys.time(), ".csv") %>%
    str_replace(" CEST", "") %>%
    str_replace_all("-", "_") %>%
    str_replace_all(" ", "_") %>%
    str_replace_all(":", "_")
}

# sortie en csv du fichier de résultat
write_csv2(df, file = fichier_csv_sortie)

}
