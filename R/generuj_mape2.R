#' Generowanie map statystycznych (Choropleth) dla Polski, Europy i Świata
#'
#' Funkcja tworzy estetyczne mapy tematyczne (choropleth) z inteligentnym rozmieszczaniem etykiet.
#' Automatycznie dobiera kontrast koloru tekstu do tła, obsługuje polskie nazewnictwo jednostek
#' administracyjnych oraz pozwala na łatwą agregację do poziomu makroregionów.
#'
#' @param data Ramka danych (data.frame) zawierająca kolumny z nazwami regionów oraz wartościami.
#' @param zasieg Charakter określający obszar: "polska_woj" (województwa), "polska_makro" (makroregiony),
#'        "europa" lub "swiat".
#' @param zmienna_wartosc Nazwa kolumny w `data` zawierającej wartości liczbowe do zmapowania.
#' @param sufiks Opcjonalny ciąg znaków doklejany do wartości liczbowej na etykiecie (np. "%", " zł").
#'        Domyślnie NULL (brak sufiksu).
#' @param tytul Tytuł główny wykresu (opcjonalnie).
#' @param podtytul Podtytuł wykresu (opcjonalnie).
#' @param podpis Tekst podpisu (źródło) wyświetlany w prawym dolnym rogu.
#' @param paleta Wektor kolorów (np. `c("white", "blue")`) definiujący gradient kolorystyczny mapy.
#' @param pokaz_legenda Logiczne, czy wyświetlać legendę kolorów. Domyślnie FALSE.
#' @param tytul_legendy Nazwa wyświetlana nad legendą.
#' @param pozycja_legendy Pozycja legendy na wykresie: "right", "left", "top", "bottom".
#' @param kolor_na Kolor wypełnienia dla obszarów, dla których nie podano danych (NA). Domyślnie "gray80".
#' @param kolor_obrysu Kolor linii granic między obszarami. Domyślnie "white".
#' @param grubosc_linii Szerokość linii granic między obszarami. Domyślnie 0.1.
#' @param pokaz_na_etykiety Logiczne, czy wyświetlać etykiety tekstowe dla obszarów bez danych. Domyślnie FALSE.
#' @param sila_odpychania Parametr `force` dla ggrepel - siła rozsuwania nachodzących na siebie etykiet.
#' @param odstep_etykiet Parametr `box.padding` dla ggrepel - margines wokół etykiet.
#' @param segmenty Logiczne, czy rysować linie pomocnicze łączące przesunięte etykiety z ich regionami.
#' @param prog_jasnosci Wartość (0-100) progu jasności tła (HCL), powyżej którego kolor tekstu
#'        zmienia się z białego na czarny.
#' @param obwoluta_blask Grubość konturu (halo) wokół tekstu etykiety. Jeśli NULL (domyślnie),
#'        etykiety są wyświetlane bez obwoluty.
#'
#' @details
#' W przypadku map Polski (`polska_woj`), funkcja automatycznie:
#' \itemize{
#'   \item Mapuje polskie nazwy województw (również bez przedrostka "województwo").
#'   \item Wyświetla etykiety województw małymi literami.
#'   \item Dla makroregionów wstawia znak nowej linii po słowie "makroregion".
#' }
#' W przypadku map Europy i Świata, etykiety są centrowane na największym obszarze lądowym
#' (pomijając terytoria zamorskie i wyspy), co poprawia czytelność (np. dla Francji czy Norwegii).
#'
#' @return Obiekt klasy ggplot.
#'
#' @import ggplot2
#' @import dplyr
#' @import sf
#' @import ggrepel
#' @import rnaturalearth
#' @import farver
#' @import scales
#'
#' @export

generuj_mape <- function(data,
						 zasieg = "polska_woj",
						 zmienna_wartosc = "wartosc",
						 sufiks = NULL,              # NOWY ARGUMENT
						 tytul = NULL,
						 podtytul = NULL,
						 podpis = NULL,
						 paleta = c("#e1f5fe", "#01579b"),
						 pokaz_legenda = FALSE,
						 tytul_legendy = "Skala",
						 pozycja_legendy = "right",
						 kolor_na = "gray80",
						 kolor_obrysu = "white",
						 grubosc_linii = 0.1,
						 pokaz_na_etykiety = FALSE,
						 sila_odpychania = 0,
						 odstep_etykiet = 0.3,
						 segmenty = FALSE,
						 prog_jasnosci = 50,
						 obwoluta_blask = NULL) {

	suppressPackageStartupMessages({
		library(rnaturalearth)
		library(rnaturalearthdata)
		library(sf)
		library(ggplot2)
		library(dplyr)
		library(farver)
		library(ggrepel)
		library(scales)
	})

	# 1. Pobieranie bazy mapy
	mapa_base <- if(zasieg == "swiat") {
		ne_countries(scale = "medium", returnclass = "sf")
	} else if(zasieg == "europa") {
		ne_countries(scale = "medium", returnclass = "sf") %>%
			suppressWarnings(st_crop(xmin = -15, ymin = 33, xmax = 45, ymax = 71))
	} else {
		ne_states(country = "poland", returnclass = "sf")
	}

	if(zasieg %in% c("polska_woj", "polska_makro")) {
		mapa_base$name_join <- tolower(mapa_base$name_pl)
	} else {
		mapa_base$name_join <- mapa_base$name
	}

	# 2. Standaryzacja danych użytkownika
	if(zasieg %in% c("polska_woj", "polska_makro")) {
		data$region <- tolower(data$region)
		if(zasieg == "polska_woj") {
			data$region <- ifelse(!grepl("województwo", data$region),
								  paste("województwo", data$region), data$region)
		}
	}

	# 3. Agregacja makroregionów
	if (zasieg == "polska_makro") {
		slownik <- data.frame(
			name_join = tolower(c("województwo łódzkie", "województwo świętokrzyskie", "województwo małopolskie",
								  "województwo śląskie", "województwo lubelskie", "województwo podkarpackie",
								  "województwo podlaskie", "województwo lubuskie", "województwo zachodniopomorskie",
								  "województwo wielkopolskie", "województwo dolnośląskie", "województwo opolskie",
								  "województwo kujawsko-pomorskie", "województwo pomorskie", "województwo warmińsko-mazurskie",
								  "województwo mazowieckie")),
			region_final = tolower(c(rep("Makroregion Centralny", 2), rep("Makroregion Południowy", 2),
									 rep("Makroregion Wschodni", 3), rep("Makroregion Północno-Zachodni", 3),
									 rep("Makroregion Południowo-Zachodni", 2), rep("Makroregion Północny", 3),
									 "Makroregion Województwo Mazowieckie")))

		mapa_base <- mapa_base %>%
			left_join(slownik, by = "name_join") %>%
			filter(!is.na(region_final)) %>%
			group_by(region_final) %>%
			summarise(geometry = st_union(geometry), .groups = "drop") %>%
			rename(name_join = region_final)
	}

	# 4. Łączenie z danymi i obliczenia kolorów
	mapa_data <- mapa_base %>% left_join(data, by = c("name_join" = "region"))
	wartosci_skali <- mapa_data[[zmienna_wartosc]]
	map_range <- if(all(is.na(wartosci_skali))) c(0, 1) else range(wartosci_skali, na.rm = TRUE)
	col_func <- scales::col_numeric(palette = paleta, domain = map_range, na.color = kolor_na)

	# 5. Centroidy i etykiety
	mapa_glowne_czesci <- mapa_data %>%
		st_make_valid() %>%
		st_cast("POLYGON") %>%
		mutate(area = st_area(geometry)) %>%
		group_by(name_join) %>%
		filter(area == max(area)) %>%
		ungroup()

	centroidy <- suppressWarnings(st_centroid(mapa_glowne_czesci))
	centroidy <- cbind(centroidy, st_coordinates(centroidy))

	centroidy <- centroidy %>%
		mutate(
			fill_actual = col_func(.data[[zmienna_wartosc]]),
			lum = farver::get_channel(fill_actual, "l", space = "hcl"),
			text_color = ifelse(lum < prog_jasnosci, "white", "black"),
			glow_color = ifelse(text_color == "white", "black", "white"),

			# LOGIKA TEKSTU NAZWY
			label_text = if(zasieg == "polska_woj") {
				tolower(gsub("(?i)województwo\\s*", "", name_join))
			} else {
				gsub("(?i)makroregion\\s*", "makroregion\n", name_join)
			},

			# LOGIKA WARTOŚCI + SUFIKS
			val_label = if(is.null(sufiks)) {
				as.character(.data[[zmienna_wartosc]])
			} else {
				paste0(.data[[zmienna_wartosc]], sufiks)
			},

			label_display = ifelse(is.na(.data[[zmienna_wartosc]]), NA,
								   paste0(label_text, "\n", val_label))
		)

	if (!pokaz_na_etykiety) centroidy <- centroidy %>% filter(!is.na(label_display))

	# 6. Budowa wykresu
	p <- ggplot(mapa_data) +
		geom_sf(aes(fill = .data[[zmienna_wartosc]]), color = kolor_obrysu, size = grubosc_linii) +
		geom_text_repel(
			data = centroidy,
			aes(x = X, y = Y, label = label_display),
			color = centroidy$text_color,
			bg.color = if(!is.null(obwoluta_blask)) centroidy$glow_color else NA,
			bg.r = if(!is.null(obwoluta_blask)) obwoluta_blask else 0,
			size = 3.2, lineheight = 0.8,
			box.padding = odstep_etykiet, force = sila_odpychania,
			min.segment.length = if(segmenty) 0 else Inf,
			max.overlaps = 100, inherit.aes = FALSE
		) +
		theme_void() +
		labs(title = tytul, subtitle = podtytul, caption = podpis, fill = tytul_legendy) +
		theme(legend.position = ifelse(pokaz_legenda, pozycja_legendy, "none"))

	if (zasieg == "europa") p <- p + coord_sf(xlim = c(-15, 42), ylim = c(34, 71), expand = FALSE)

	if (length(paleta) == 2) {
		p <- p + scale_fill_gradient(low = paleta[1], high = paleta[2], na.value = kolor_na)
	} else {
		p <- p + scale_fill_stepsn(colors = paleta, na.value = kolor_na)
	}

	return(p)
}
