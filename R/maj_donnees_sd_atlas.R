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
#' @importFrom readr read_csv2 write_csv2
#' @importFrom dplyr mutate_at vars starts_with rename left_join select mutate everything
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
# Lecture du fichier shapefile à jour
  sd_a_jour <- mef_donnees_sd_atlas(fichier_shp_a_jour = fichier_shp_a_jour)

# lecture du fichier csv du millésime précédent
  sd_ancien <- read_csv2(file = fichier_csv_precedent) %>%
    mutate_at(vars(starts_with("date")), as.character) %>%
    rename(unique_obs_id_old = unique_obs_id)

# assemblage des deux
  assemblage <- sd_a_jour %>%
    rename(unique_obs_id_new = unique_obs_id) %>%
    left_join(sd_ancien, by = c("unique_dataset_id", "unique_ope_id", "date_peche",
                                "code_exutoire", "code_station", "localisation", "x_wgs84", "y_wgs84",
                                "ctxte_peche", "code_espece", "esp_code_taxref")) %>%
    select(unique_obs_id_new, unique_obs_id, everything()) %>%
    select(-effectif.y) %>%
    rename(effectif = effectif.x)

# gestion des uuid. Si l'observation est nouvelle, il est créé, sinon il est conservé
# mêrme en cas de changement de statut de présence / absence
  assemblage <- assemblage %>%
    mutate(uuid = ifelse(is.na(unique_obs_id), unique_obs_id_new, unique_obs_id)) %>%
    select(-unique_obs_id, -unique_obs_id_new) %>%
    rename(unique_obs_id = uuid) %>%
    select(unique_obs_id,
           everything())

# sortie en csv du fichier de résultat
  write_csv2(sd_ancien, file = fichier_csv_a_jour)



}

