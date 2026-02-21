#' Wyświetl podgląd palet kolorów NASK
#'
#' Funkcja generuje wykres (swatch plot) prezentujący wszystkie dostępne
#' w pakiecie palety kolorów, co ułatwia ich wybór do wizualizacji.
#'
#' @return Obiekt ggplot2 prezentujący podgląd palet.
#'
#' @import ggplot2
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map_df
#' @export
#'
#' @examples
#' show_nask_palettes()
show_nask_palettes <- function() {
  require(ggplot2)
  # Lista palet do pokazania
  pals <- list(
    nominalne_8  = kolory_nominalne_8,
    nominalne_12 = kolory_nominalne_12,
    unipolar_4 = kolory_unipolar_4,
    unipolar_5   = kolory_unipolar_5,
    unipolar_7 = kolory_unipolar_7,
    unipolar_11  = kolory_unipolar_11,
    bipolar_4    = kolory_bipolar_4,
    bipolar_5    = kolory_bipolar_5,
    bipolar_7    = kolory_bipolar_7,
    bipolar_11   = kolory_bipolar_11,
    diverging_4  = kolory_diverging_4,
    diverging_5  = kolory_diverging_5,
    diverging_7  = kolory_diverging_7,
    diverging_11 = kolory_diverging_11,
    #highlight = kolory_highlight,
    techniczne   = kolory_techniczne#,
    #'kolor NA' = kolor_NA
  )

  # Przygotowanie danych do wykresu
  df <- lapply(names(pals), function(nm) {
    data.frame(
      paleta = nm,
      id = seq_along(pals[[nm]]),
      kolor = pals[[nm]],
      stringsAsFactors = FALSE
    )
  })

  print(df)

  df <- do.call(rbind, df)

  # Generowanie wykresu
  ggplot(df, aes(x = id, y = paleta, fill = kolor)) +
    geom_tile(color = "white", linewidth = 0.5) +
    scale_fill_identity() +
    geom_text(aes(label = kolor),
              color = "white",
              size = 2.5,
              angle = 90,
              alpha = 0.8) +
    labs(
      title = "Podgląd palet kolorów NASK",
      x = "Indeks koloru w wektorze",
      y = "Nazwa palety"
    ) +
    theme_nask()
}
