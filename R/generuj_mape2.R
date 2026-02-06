#' Generowanie map statystycznych (Choropleth) dla Polski, Europy i Świata
#'
#' Funkcja tworzy estetyczne mapy tematyczne z automatycznym dobieraniem kontrastu etykiet,
#' obsługą brakujących danych (NA) oraz inteligentnym rozmieszczaniem tekstów (ggrepel).
#'
#' @param data Ramka danych (data.frame) zawierająca kolumny z nazwami regionów oraz wartościami.
#' @param zasieg Charakter określający obszar: "polska_woj", "polska_makro", "europa" lub "swiat".
#' @param zmienna_wartosc Nazwa kolumny w `data` zawierającej wartości liczbowe do zmapowania.
#' @param tytul Tytuł główny wykresu (opcjonalnie).
#' @param podtytul Podtytuł wykresu (opcjonalnie).
#' @param podpis Tekst podpisu (źródło) w prawym dolnym rogu.
#' @param paleta Wektor kolorów (np. `c("white", "blue")`) definiujący gradient lub skalę krokową.
#' @param pokaz_legenda Logiczne, czy wyświetlać legendę kolorów.
#' @param tytul_legendy Nazwa wyświetlana nad legendą.
#' @param pozycja_legendy Pozycja legendy: "right", "left", "top", "bottom".
#' @param kolor_na Kolor wypełnienia dla obszarów, dla których nie podano danych (domyślnie "gray80").
#' @param kolor_obrysu Kolor linii granic między obszarami (domyślnie "white").
#' @param grubosc_linii Szerokość linii granic (domyślnie 0.5).
#' @param pokaz_na_etykiety Logiczne, czy wyświetlać etykiety tekstowe dla obszarów z brakiem danych.
#' @param sila_odpychania Parametr `force` dla ggrepel - siła rozsuwania nachodzących na siebie etykiet.
#' @param odstep_etykiet Parametr `box.padding` dla ggrepel - margines wokół etykiet.
#' @param segmenty Logiczne, czy rysować linie pomocnicze od przesuniętych etykiet do obszarów.
#' @param prog_jasnosci Wartość (0-100) progu jasności tła, powyżej którego kolor tekstu zmienia się z białego na czarny.
#' @param obwoluta_blask Grubość konturu (halo) wokół tekstu etykiety dla poprawy czytelności.
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
#'
#' @examples
#' \dontrun{
#' dane <- data.frame(region = c("Masovian", "Silesian"), wynik = c(95, 70))
#' generuj_mape(dane, zasieg = "polska_woj", zmienna_wartosc = "wynik", kolor_na = "white")
#' }
