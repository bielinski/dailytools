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
						 proporcja = NULL) {

	require(terra)
	require(tidyterra)
	require(ggplot2)
	require(dplyr)
	require(farver)

	# 1. Definicja kodów ISO3 dla Europy
	kody_europy <- c(
		"ALB", "AND", "AUT", "BEL", "BGR", "BIH", "BLR", "CHE", "CYP", "CZE",
		"DEU", "DNK", "ESP", "EST", "FIN", "FRA", "GBR", "GIB", "GRC", "HRV",
		"HUN", "IRL", "ISL", "ITA", "KOS", "LIE", "LTU", "LUX", "LVA", "MCO",
		"MDA", "MKD", "MLT", "MNE", "NLD", "NOR", "POL", "PRT", "ROU", "RUS",
		"SMR", "SRB", "SVK", "SVN", "SWE", "TUR", "UKR", "VAT"
	)

	# 2. Wczytanie geometrii
	sciezka <- switch(poziom,
					  "polska"      = 'maps/humdata/pol_admin0.shp',
					  "wojewodztwa" = 'maps/humdata/pol_admin1.shp',
					  "powiaty"     = 'maps/humdata/pol_admin2.shp',
					  "gminy"       = 'maps/humdata/pol_admin3.shp',
					  "europa"      = 'maps/CNTR_RG_20M_2024_4326/CNTR_RG_20M_2024_4326.shp',
					  "swiat"       = 'maps/CNTR_RG_20M_2024_4326/CNTR_RG_20M_2024_4326.shp',
					  stop("Nieznany poziom!")
	)

	mapa <- vect(sciezka)

	# 3. Filtrowanie (Rosja zostaje!)
	if (poziom == "europa") {
		mapa <- mapa %>% filter(ISO3_CODE %in% kody_europy)
	}

	# 4. Projekcja
	if (poziom %in% c("polska", "wojewodztwa", "powiaty", "gminy")) {
		mapa <- project(mapa, "EPSG:2180")
	} else {
		mapa <- project(mapa, "EPSG:4326")
	}

	# 5. Klucze dla joinów
	klucz_mapy <- if(poziom %in% c("europa", "swiat")) "ISO3_CODE" else switch(poziom,
																			   "wojewodztwa" = "adm1_pcode",
																			   "powiaty"     = "adm2_pcode",
																			   "gminy"       = "adm3_pcode",
																			   "polska"      = "adm0_pcode"
	)

	# 6. Join danych
	if (!is.null(dane_zewnetrzne) && !is.null(kolumna_id)) {
		dane_do_joinu <- dane_zewnetrzne %>% rename(!!klucz_mapy := all_of(kolumna_id))
		mapa <- mapa %>% left_join(dane_do_joinu, by = klucz_mapy)
	}

	# 7. Budowa wykresu
	p <- ggplot(mapa)

	if (!is.null(kolumna_wartosc)) {
		p <- p + geom_spatvector(aes(fill = .data[[kolumna_wartosc]]), color = "white", lwd = 0.1)

		if (pokaz_etykiety) {
			p <- p + geom_spatvector_text(
				aes(label = .data[[kolumna_wartosc]],
					colour = after_scale(ifelse(farver::get_channel(fill, "l", space = "hcl") < 60, "white", "black"))),
				size = 2.5, check_overlap = TRUE, fontface = "bold"
			)
		}

		if (paleta == "custom" || !all(kolory_custom == c("white", "red"))) {
			p <- p + scale_fill_gradient(low = kolory_custom[1], high = kolory_custom[2], na.value = "grey80", name = "Wartość")
		} else {
			p <- p + scale_fill_viridis_c(option = paleta, na.value = "grey80", name = "Wartość")
		}
	} else {
		p <- p + geom_spatvector(fill = "lightblue", color = "white", lwd = 0.1)
	}

	# --- NOWE WSPÓŁRZĘDNE DLA EUROPY ---
	if (poziom == "europa") {
		# xlim: od -25 (Islandia) do 75 (europejska część Rosji)
		# ylim: od 28 (północna Afryka / Wyspy Kanaryjskie) do 72 (północ kontynentu)
		p <- p + coord_sf(xlim = c(-25, 43), ylim = c(34, 72), expand = FALSE)
	}

	# 8. Labs i styl
	p <- p + theme_void() +
		theme(legend.position = "bottom",
			  aspect.ratio = proporcja) + # Tu stosujemy nasze rozciągnięcie) +
		labs(
			title = if(!is.null(m_title)) as.character(m_title) else NULL,
			subtitle = if(!is.null(m_subtitle)) as.character(m_subtitle) else NULL,
			caption = if(!is.null(m_caption)) as.character(m_caption) else NULL
		)

	return(p)
}
