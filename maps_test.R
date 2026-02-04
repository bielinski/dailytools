library(dplyr)
library(ggplot2)

library(tmap)


library(rgdal)
library(rgeos)

tm_shape(nz) + tm_fill()


library(sf)
library(terra)

polska <- st_read(dsn = 'maps/polska.shp')
polska <- fortify(polska)

ggplot(polska) + geom_sf() + theme_void()


wojewodztwa <- st_read(dsn = 'maps/wojewodztwa.shp')
ggplot(wojewodztwa) + geom_sf()

powiaty <- st_read('maps/powiaty.shp')
ggplot(powiaty) + geom_sf()

# terra

polska <- vect(x = 'maps/humdata/pol_admin0.shp')
polska
plot(polska)

wojewodztwa <- vect(x = 'maps/humdata/pol_admin1.shp')
wojewodztwa
plot(wojewodztwa, 'adm1_name')

powiaty <- vect(x = 'maps//humdata/pol_admin2.shp')
plot(powiaty)


wojewodztwa_df <- as.data.frame(wojewodztwa)
wojewodztwa_df

library(terra)
library(tidyterra)
wojewodztwa %>%
  autoplot(fill = 'blue') +
  theme_void()

sample_data <- data.frame(
  adm1_pcode = as_tibble(wojewodztwa)$adm1_pcode %>% unique(),
  some_value = c(2,4,3,5,6,7,8,7,6,4,5,6,5,6,1,10)
)

wojewodztwa %>%
  left_join(sample_data, by = 'adm1_pcode') %>%
  autoplot(aes(fill = some_value)) +
  theme_void() +
  theme(legend.position = 'bottom')


# 1. Tworzymy dane testowe dokładnie tak, jak w Twoim kodzie:
sample_data <- data.frame(
  adm1_pcode = as_tibble(vect('maps/humdata/pol_admin1.shp'))$adm1_pcode %>% unique(),
  some_value = c(2,4,3,5,6,7,8,7,6,4,5,6,5,6,1,10)
)

generuj_mape(poziom = 'wojewodztwa')

generuj_mape(
  poziom = "wojewodztwa",
  dane_zewnetrzne = sample_data,
  kolumna_id = "adm1_pcode",
  kolumna_wartosc = "some_value",
  paleta = 'custom',
  kolory_custom = c('white', 'navyblue'),
  pokaz_etykiety = TRUE,
  m_title = "Moja Mapa",        # Używamy m_title
  m_subtitle = "Podtytuł",      # Używamy m_subtitle
  m_caption = "Źródło: Dane",    # Używamy m_caption
  linia_grubosc = 0.01
)


generuj_mape(poziom = 'powiaty')
generuj_mape(poziom = 'gminy')
generuj_mape(poziom = 'swiat')
generuj_mape(poziom = 'europa')

mapa <- vect('maps/CNTR_RG_20M_2024_4326/CNTR_RG_20M_2024_4326.shp')
mapa


# 1. Przygotowanie danych testowych
europa_stats <- data.frame(
  kod_kraju = c(
    "POL", "DEU", "FRA", "ITA", "ESP", "GBR", "UKR", "NOR", "SWE", "FIN",
    "ISL", "IRL", "PRT", "AUT", "CHE", "BEL", "NLD", "CZE", "SVK", "HUN",
    "ROU", "BGR", "GRC", "TUR", "EST", "LVA", "LTU", "DNK", "SRB", "HRV"
  ),
  wartosc = c(
    95, 88, 82, 75, 70, 85, 90, 98, 92, 94,
    99, 87, 65, 89, 91, 80, 86, 78, 74, 72,
    68, 60, 62, 55, 93, 84, 85, 90, 58, 77
  )
)

# 2. Wywołanie Twojej funkcji
generuj_mape(
  poziom = "europa",
  dane_zewnetrzne = europa_stats,
  kolumna_id = "kod_kraju",
  kolumna_wartosc = "wartosc",
  paleta = "viridis",
  pokaz_etykiety = TRUE,
  m_title = "Wskaźnik Innowacji w Europie",
  m_subtitle = "Dane testowe na rok 2026",
  m_caption = "Źródło: Symulacja programistyczna",
  proporcja = 0.73
)
