#' Mettre en forme les données de pêches SD OFB pour la plateforme régionale des données naturalistes
#'
#' @param fichier_shp_a_jour Caractère. Chemin vers le fichier shapefile contenant les données à jour.
#' @param passerelle_taxonomique Dataframe permettant la correspondance entre les codes espèces à trois
#'     lettres et les identifiants Taxref. Il est fourni par le package {aspe} et doit
#'     comprendre des variables nommées esp_code_alternatif, esp_code_sandre, esp_id, esp_nom_latin et
#'     esp_code_taxref.
#' @param crs_init Numérique. Code EPSG du CRS initial. Par défaut c'est 2154 (Lambert 93).
#' @param crs_fin Numérique. Code EPSG du CRS de sortie. Par défaut c'est 4326 (WGS84).
#'
#' @return Le dataframe avec les données au format souhaité (long, nommage et typage des variables).
#' @export
#'
#' @importFrom atlaspoissons get_coords recode_and_filter_species
#' @importFrom dplyr mutate select mutate_at vars bind_cols left_join across summarise ungroup rename
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_replace str_sub
#' @importFrom rgdal readOGR
#' @importFrom sf st_as_sf st_drop_geometry
#' @importFrom uuid UUIDgenerate
#' @importFrom utils data
#' @importFrom purrr set_names
#'
#' @examples
#' \dontrun{
#' data <- mef_donnees_sd_atlas(
#'   fichier_shp_a_jour = "donnees_brutes/atlas.shp")
#' }
mef_donnees_sd_atlas <- function(fichier_shp_a_jour,
                                 passerelle_taxonomique,
                                 crs_init = 2154,
                                 crs_fin = 4326)

{

  # lecture des données shp
  sd_geo <- readOGR(fichier_shp_a_jour,
                    encoding = "UTF-8") %>%
    st_as_sf()

  # collecte des coordonnées dans le bon CRS
  coords <- get_coords(sf_obj = sd_geo,
                       crs_init = crs_init,
                       crs_fin = crs_fin)

  # ajout des colonnes de coordonnées au dataframe, passage en format long et renommage
  # NB on ajoute l'uuid du jeu de données qui est identique pour ttes les observations
  # c'est celui indiqué sur la fiche de métadonnées metadonnee_jeu_donnees.docx
  df <- sd_geo %>%
    st_drop_geometry() %>%
    bind_cols(coords) %>%
    pivot_longer(cols = ABH:VAX,
                 names_to = "code_espece",
                 values_to = "effectif") %>%
    mutate(code_station = NA,
           organisme = "SD OFB",
           type_peche = "Atlas",
           annee = str_sub(Date_peche, 1, 4),
           annee = as.integer(annee),
           effectif = as.integer(effectif),
           unique_dataset_id = "1e727cde-8ce3-48c2-874f-b76a5eca9e5f",
           ope_id = str_replace(uuid, '\\{', ''),
           ope_id = str_replace(ope_id, '\\}', ''))

  # gestion de qq cas particuliers de codes
  df <- df %>%
    recode_and_filter_species()  %>%
    mutate(code_espece = ifelse(code_espece %in% c("VAX", "VAN"),
                                yes = "VAR",
                                no = code_espece))

  # agrégation pour éviter des doublons s'il y a eu des regroupements de taxons
  df <- df %>%
    group_by(across(c(-effectif))) %>%
    summarise(effectif = max(effectif, na.rm = TRUE)) %>%
    ungroup()

  # génération des UUID (ne fonctionne pas à l'intérieur d'un mutate auquel cas ttes obs ont le même uuid)
  unique_obs_id <- UUIDgenerate(n = nrow(df)) %>%
    as.data.frame() %>%
    set_names("unique_obs_id")

  # ajout des UUID, renommage et mise en ordre des colonnes
  df <- df %>%
    bind_cols(unique_obs_id) %>%
    select(unique_obs_id,
           unique_dataset_id,
           unique_ope_id = ope_id,
           date_peche = Date_peche,
           date_saisie = Date_saisi,
           date_modif = Date_modif,
           code_exutoire = IDD,
           code_station,
           localisation = Bassin,
           x_wgs84,
           y_wgs84,
           ctxte_peche = type_peche,
           code_espece,
           effectif) %>%
    mutate_at(vars(code_station, localisation, date_peche),
              as.character)



  # ajout des codes taxref ; gestion des vandoises indéterminées VAX (slt des VAR en Bretagne)
  df <- df %>%
    left_join(y = passerelle_taxonomique %>%
                rename(code_espece = esp_code_alternatif)) %>%
    select(-esp_code_sandre, -esp_id, -esp_nom_latin)

  df

}

