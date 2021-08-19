library(tidyverse)

detach("package:atlas", unload = TRUE)
library(atlas)

rm(list = ls())

############## Données SD
sd_file <- "raw_data/peche_georect_sd_2015_2019_20210818.shp"
sd_base <- sf::st_read(dsn = sd_file)
mapview::mapview(sd_base)

sd <- atlas::clean_sd(sd_base) 

# sd <- sd_base %>% 
#   mutate(across(where(is.numeric), ~na_if(., 0)))
# 




sd <- atlas::clean_sd(sd_base) 
# especes_a_supprimer <- c("PCC", "ASL", "OCI", "ECR", "MAH", "PCF", "OCV", "ASA",
#                          "APP", "APT", "OCL", "GOX", "VAL", "POB", "CRE", "CRC", "GRV",
#                          "GRT", "GRI", "LOU", "MUP", "PLI", "ALF", "BRX")

# Comme il peut y avoir des regroupements de taxons (ex parmi les carpes), ré-aggrège par code sp
sd <- sd %>% 
  recode_and_filter_species () %>% 
  group_by(x_wgs84, y_wgs84, localisation, type_peche, date_peche, code_espece) %>% 
      summarise(effectif = sum(effectif, na.rm = TRUE)) %>% 
  ungroup() %>%
  filter(effectif > 0) %>% 
  select(-effectif) %>% 
  rename(esp_code_alternatif = code_espece)

# Ajout des codes taxonomiques taxref
load("processed_data/ref_espece.RData")

ref_espece <- ref_espece %>% 
  select(esp_code_aspe, cd_nom, esp_code_alternatif, esp_nom_commun, esp_nom_latin)

sd <- sd %>% 
  left_join(y = ref_espece)

#---------------------------------------------------------
# Vérifications
#---------------------------------------------------------

### Carto richesse et liste de taxons
sta_geo <- sd %>% 
  select(localisation, x_wgs84, y_wgs84) %>% 
  unique() %>% 
  arrange(localisation, x_wgs84, y_wgs84) %>% 
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326)

popups <- sd %>% 
  pivot_wider(id_cols = x_wgs84:localisation,
              names_from = esp_code_alternatif,
              values_from = esp_code_alternatif) %>% 
  arrange(localisation, x_wgs84, y_wgs84) %>%
  tidyr::unite("Espèces", ANG:VAX, sep = ", ", na.rm = TRUE) %>% 
  mutate(Richesse = 1 + stringr::str_count(`Espèces`, pattern = ", ")) %>% 
  select(-localisation)

sta_geo <- cbind(sta_geo, popups)

mapview::mapview(sta_geo,
                 cex = "Richesse",
                 popup = leafpop::popupTable(sta_geo, zcol = c("localisation", "Richesse", "Espèces")))

# les bassins versants
# à partir de la couche de Josselin sur
# Z:/dr35_projets/PROJETS/ATLAS_POISSONS/ATLAS_SIG/atlas_piscicole_bretagne_20200220/layers
# comme il y a pb d'encodage UTF-8 avec st_read(), utilisation de rgdal::readOGR() puis st_as_sf()

bassins <- rgdal::readOGR("raw_data/bv_20200132_indicateurs.shp",
                          use_iconv = TRUE, 
                          encoding = "latin1") %>%
  st_as_sf() %>% 
  `st_crs<-`(2154) %>% 
  st_transform(crs = 4326) %>% 
  rename(code_exutoire = IDD) %>% 
  filter(TRUE)

# carte
mapview::mapview(bassins,
                 col.regions = wesanderson::wes_palette("Royal2")[5],
                 alpha.regions = 0.1) +
  mapview::mapview(sta_geo,
                   col.regions = wesanderson::wes_palette("Royal1")[2],
                   cex = "Richesse",
                   popup = leafpop::popupTable(sta_geo, zcol = c("localisation", "Richesse", "Espèces")))


### Carto par espèce
sd_geo <- sd %>% 
  sf::st_as_sf(coords = c("x_wgs84", "y_wgs84"), crs = 4326)

mapview::mapview(sd_geo,
                 burst = "esp_code_alternatif",
                 hide = TRUE)







save(sd, file = 'processed_data/sd.RData')
save(sd_large, file = 'processed_data/sd_large.RData')

rm(sd_base, especes_a_supprimer, sd_file)
