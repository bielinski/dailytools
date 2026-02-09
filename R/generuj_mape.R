#' Uniwersalne generowanie map wektorowych
#'
#' Funkcja pozwala na tworzenie estetycznych map Polski (na poziomach: kraj, województwa, powiaty, gminy),
#' Europy oraz Świata. Obsługuje nakładanie danych zewnętrznych, automatyczny kontrast etykiet
#' oraz personalizację wyglądu (kolory, grubość linii, proporcje). Obsługuje dane numeryczne i kategoryczne.
#'
#' @param poziom Charakter; poziom szczegółowości mapy: "polska", "wojewodztwa", "powiaty", "gminy", "europa", "swiat".
#' @param dane_zewnetrzne Data frame; ramka danych zawierająca wartości do naniesienia na mapę. Domyślnie NULL.
#' @param kolumna_id Charakter; nazwa kolumny w `dane_zewnetrzne`, która zawiera kody identyfikacyjne (np. ISO3 lub PCODE).
#' @param kolumna_wartosc Charakter; nazwa kolumny z wartościami (numeryczne lub factor/character).
#' @param paleta Charakter; nazwa palety viridis ("magma", "inferno", "plasma", "viridis", "cividis", "mako", "rocket", "turbo") lub "custom".
#' @param kolory_custom Wektor; kolory dla gradientu (2 kolory) lub paleta dla kategorii (wiele kolorów).
#' @param m_title Charakter; tytuł główny mapy.
#' @param m_subtitle Charakter; podtytuł mapy.
#' @param m_caption Charakter; podpis pod mapą (np. źródło danych).
#' @param pokaz_etykiety Logiczne; czy wyświetlać wartości/nazwy bezpośrednio na mapie.
#' @param proporcja Numeryczne; stosunek osi Y do X (aspect ratio).
#' @param linia_grubosc Numeryczne; grubość linii granic poligonów (standardowo 0.2).
#' @param na_kolor Charakter; kolor dla braków danych (wartości NA).
#' @param legenda Logiczne; czy wyświetlać legendę.
#' @param legend.position Character; pozycja legendy ('bottom', 'top', 'left', 'right').
#'
#' @export
generuj_mape_old <- function(poziom = "wojewodztwa",
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
						 linia_grubosc = 0.2,
						 na_kolor = 'gray85',
						 legenda = TRUE,
						 legend.position = 'bottom') {

	require(terra)
	require(tidyterra)
	require(ggplot2)
	require(dplyr)
	require(farver)

	# 1. Walidacja danych wejściowych
	if (is.function(dane_zewnetrzne)) {
		stop("BŁĄD: Argument 'dane_zewnetrzne' otrzymał funkcję zamiast danych. Prawdopodobnie używasz nazwy 'df', która w R jest funkcją systemową. Zmień nazwę swojej ramki danych (np. na 'moje_dane').")
	}

	# Definicja tekstów (bezpieczne wyciąganie wartości)
	tit <- if(!is.null(m_title)) paste(m_title) else NULL
	sub <- if(!is.null(m_subtitle)) paste(m_subtitle) else NULL
	cap <- if(!is.null(m_caption)) paste(m_caption) else NULL

	# 2. Wczytanie i projekcja
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
		if("ISO3_CODE" %in% names(mapa)) {
			mapa <- mapa[mapa$ISO3_CODE %in% kody_europy, ]
		}
		mapa <- project(mapa, "EPSG:4326")
	} else if (poziom == "swiat") {
		mapa <- project(mapa, "EPSG:4326")
	} else {
		mapa <- project(mapa, "EPSG:2180")
	}

	# 3. Łączenie danych
	klucz_mapy <- if(poziom %in% c("europa", "swiat")) "ISO3_CODE" else switch(poziom, "wojewodztwa"="adm1_pcode", "powiaty"="adm2_pcode", "gminy"="adm3_pcode", "polska"="adm0_pcode")

	if (!is.null(dane_zewnetrzne) && !is.null(kolumna_id)) {
		dane_do_joinu <- as.data.frame(dane_zewnetrzne) %>%
			dplyr::rename(!!klucz_mapy := all_of(kolumna_id))
		mapa <- mapa %>% left_join(dane_do_joinu, by = klucz_mapy)
	}

	# 4. Budowa wykresu
	if (!is.null(kolumna_wartosc)) {

		# Wyciągamy wartości jako czysty wektor
		wartosci_wektor <- values(mapa)[[kolumna_wartosc]]
		czy_liczba <- is.numeric(wartosci_wektor)

		# Wymuszamy factor dla danych kategorycznych na poziomie obiektu SpatVector
		if(!czy_liczba) {
			values(mapa)[[kolumna_wartosc]] <- as.factor(wartosci_wektor)
		}

		# Mapujemy fill globalnie w ggplot()
		p <- ggplot(mapa, aes(fill = .data[[kolumna_wartosc]])) +
			geom_spatvector(color = "white", linewidth = linia_grubosc)

		if (pokaz_etykiety) {
			p <- p + suppressWarnings(
				geom_spatvector_text(
					aes(label = as.character(.data[[kolumna_wartosc]]),
						colour = after_scale(ifelse(farver::get_channel(fill, "l", space = "hcl") < 50, "white", "black"))),
					size = 3, check_overlap = TRUE, fontface = "bold"
				)
			)
		}

		# Skale kolorystyczne
		if (czy_liczba) {
			if (paleta == "custom") {
				p <- p + scale_fill_gradient(low = kolory_custom[1], high = kolory_custom[2], na.value = na_kolor, name = "Wartość")
			} else {
				p <- p + scale_fill_viridis_c(option = paleta, na.value = na_kolor, name = "Wartość")
			}
		} else {
			if (paleta == "custom") {
				p <- p + scale_fill_manual(values = kolory_custom, na.value = na_kolor, name = "Kategoria")
			} else {
				p <- p + scale_fill_viridis_d(option = paleta, na.value = na_kolor, name = "Kategoria")
			}
		}

	} else {
		p <- ggplot(mapa) +
			geom_spatvector(fill = "lightblue", color = "white", linewidth = linia_grubosc)
	}

	# 5. Kadrowanie i proporcje
	if (poziom == "europa") {
		p <- p + coord_sf(xlim = c(-25, 43), ylim = c(34, 72), expand = FALSE)
	}

	p <- p + theme_void() +
		theme(aspect.ratio = proporcja) +
		labs(title = tit, subtitle = sub, caption = cap)

	if(legenda == TRUE) {
		p <- p + theme(legend.position = legend.position)
	} else {
		p <- p + theme(legend.position = 'none')
	}

	return(p)
}
