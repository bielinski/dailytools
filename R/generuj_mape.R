generuj_mape <- function(poziom = "wojewodztwa",
						 dane_zewnetrzne = NULL,
						 kolumna_id = NULL,
						 kolumna_wartosc = NULL,
						 paleta = "viridis",
						 kolory_custom = c("white", "red"),
						 m_title = NULL,
						 m_subtitle = NULL,
						 m_caption = NULL,
						 pokaz_etykiety = FALSE,
						 proporcja = NULL,
						 linia_grubosc = 0.2) { # NOWY ARGUMENT

	require(terra)
	require(tidyterra)
	require(ggplot2)
	require(dplyr)
	require(farver)

	# 1. Definicja tekstów (bezpieczne wyciąganie wartości)
	tit <- if(!is.null(m_title)) paste(m_title) else NULL
	sub <- if(!is.null(m_subtitle)) paste(m_subtitle) else NULL
	cap <- if(!is.null(m_caption)) paste(m_caption) else NULL

	# 2. Wczytanie i projekcja (bez zmian)
	sciezka <- switch(poziom,
					  "polska"      = 'maps/humdata/pol_admin0.shp',
					  "wojewodztwa" = 'maps/humdata/pol_admin1.shp',
					  "powiaty"     = 'maps/humdata/pol_admin2.shp',
					  "gminy"       = 'maps/humdata/pol_admin3.shp',
					  "europa"      = 'maps/CNTR_RG_20M_2024_4326/CNTR_RG_20M_2024_4326.shp',
					  "swiat"       = 'maps/CNTR_RG_20M_2024_4326/CNTR_RG_20M_2024_4326.shp'
	)

	mapa <- vect(sciezka)

	if (poziom == "europa") {
		kody_europy <- c("ALB", "AND", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE",
						 "DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GIB", "GRC", "HRV",
						 "HUN", "IRL", "ISL", "ITA", "KOS", "LIE", "LTU", "LUX", "LVA", "MCO",
						 "MDA", "MKD", "MLT", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS",
						 "SMR", "SRB", "SVK", "SVN", "SWE", "TUR", "UKR", "VAT")
		mapa <- mapa %>% filter(ISO3_CODE %in% kody_europy)
		mapa <- project(mapa, "EPSG:4326")
	} else if (poziom == "swiat") {
		mapa <- project(mapa, "EPSG:4326")
	} else {
		mapa <- project(mapa, "EPSG:2180")
	}

	# 3. Join danych
	klucz_mapy <- if(poziom %in% c("europa", "swiat")) "ISO3_CODE" else switch(poziom, "wojewodztwa"="adm1_pcode", "powiaty"="adm2_pcode", "gminy"="adm3_pcode", "polska"="adm0_pcode")

	if (!is.null(dane_zewnetrzne) && !is.null(kolumna_id)) {
		dane_do_joinu <- dane_zewnetrzne %>% rename(!!klucz_mapy := all_of(kolumna_id))
		mapa <- mapa %>% left_join(dane_do_joinu, by = klucz_mapy)
	}

	# 4. Budowa wykresu z obsługą linia_grubosc
	if (!is.null(kolumna_wartosc)) {
		# 'fill' w aes() na poziomie ggplot pozwala after_scale działać w etykietach
		p <- ggplot(mapa, aes(fill = .data[[kolumna_wartosc]])) +
			geom_spatvector(color = "white", linewidth = linia_grubosc)

		if (pokaz_etykiety) {
			p <- p + suppressWarnings(
				geom_spatvector_text(
					aes(label = .data[[kolumna_wartosc]],
						colour = after_scale(ifelse(farver::get_channel(fill, "l", space = "hcl") < 50, "white", "black"))),
					size = 3, check_overlap = TRUE, fontface = "bold"
				)
			)
		}

		if (paleta == "custom") {
			p <- p + scale_fill_gradient(low = kolory_custom[1], high = kolory_custom[2], na.value = "grey80", name = "Wartość")
		} else {
			p <- p + scale_fill_viridis_c(option = paleta, na.value = "grey80", name = "Wartość")
		}
	} else {
		# Pusta mapa
		p <- ggplot(mapa) +
			geom_spatvector(fill = "lightblue", color = "white", linewidth = linia_grubosc)
	}

	# 5. Kadrowanie i proporcje
	if (poziom == "europa") {
		p <- p + coord_sf(xlim = c(-25, 43), ylim = c(34, 72), expand = FALSE)
	}

	p <- p + theme_void() +
		theme(legend.position = "bottom", aspect.ratio = proporcja) +
		labs(title = tit, subtitle = sub, caption = cap)

	return(p)
}
