#' Mettre à jour les données de pêches SD OFB pour la plateforme régionale des données naturalistes
#'
#' La fonction compare une version à jour du ficj=hier de données QG
#'
#' @param df_a_jour Nom du dataframe contenant les données à jour.
#' @param df_precedent Nom du dataframe contenant les données de la précédente livraison
#'     de données à la plateforme.
#'
#' @return Le dataframe fusionné avec les données au format souhaité et les bons UUID.
#' @export
#'
#' @importFrom readr read_csv2 write_csv2
#' @importFrom dplyr mutate_at vars starts_with rename left_join select mutate everything

#'
#' @examples
#' \dontrun{
#' data <- maj_donnees_sd_atlas(
#'   fichier_shp_a_jour = "donnees_brutes/atlas.shp",
#'   fichier_csv_precedent = "donnees_traitees/2020_08_16_peches_sd_ofb_atlas.csv"
#' )
#' }
maj_donnees_sd_atlas <- function(
  df_a_jour,
  df_precedent
  )

{
# # Lecture du fichier shapefile à jour
#   df_a_jour <- mef_donnees_sd_atlas(fichier_shp_a_jour = fichier_shp_a_jour)
#
# # lecture du fichier csv du millésime précédent
#   sd_ancien <- read_csv2(file = fichier_csv_precedent) %>%
#     mutate_at(vars(starts_with("date")), as.character) %>%
#     rename(unique_obs_id_old = unique_obs_id)

# assemblage des deux
  assemblage <- df_a_jour %>%
    rename(unique_obs_id_new = unique_obs_id) %>%
    left_join(df_precedent %>%
                rename(unique_obs_id_old = unique_obs_id),
              by = c("unique_dataset_id",
                     "unique_ope_id",
                     "date_peche",
                     "code_exutoire",
                     "code_station",
                     "localisation",
                     "x_wgs84",
                     "y_wgs84",
                     "ctxte_peche",
                     "code_espece",
                     "esp_code_taxref")) %>%
    select(unique_obs_id_new,
           unique_obs_id_old,
           everything()) %>%
    select(-effectif.y) %>%
    rename(effectif = effectif.x)

# gestion des uuid. Si l'observation est nouvelle, il est créé, sinon il est conservé
# mêrme en cas de changement de statut de présence / absence
  assemblage <- assemblage %>%
    mutate(unique_obs_id = ifelse(is.na(unique_obs_id_old),
                                  yes = unique_obs_id_new,
                                  no = unique_obs_id_old)) %>%
    select(-unique_obs_id_old, -unique_obs_id_new) %>%
    select(unique_obs_id,
           everything())

  assemblage

}

