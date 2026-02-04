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
