# Tytuł 16/22 Semibold / czarny
# Podtytuł 14/20 Semibold / czarny
# Osie/etykiety 10 regular / szary
# Wartości 11,5 Semibold / czarny
# Stopka 9/12 Regular / ciemnoszary

#' Motyw graficzny zgodny ze standardem NASK
#'
#' Funkcja definiuje spójny wygląd wykresów ggplot2, ustawiając odpowiednie
#' czcionki (Archivo), kolory oraz rozmiary elementów tekstowych.
#'
#' @details
#' Motyw implementuje następujące wytyczne:
#' \itemize{
#'   \item \strong{Tytuł:} 16pt, Bold, czarny.
#'   \item \strong{Podtytuł:} 14pt, Bold, czarny.
#'   \item \strong{Osie/Etykiety:} 10pt, Regular, szary (#9D9D9D).
#'   \item \strong{Stopka:} 9pt, Regular, ciemnoszary (#757576).
#' }
#'
#' @return Zwraca obiekt klasy \code{theme}, który można dodać do wykresu ggplot2.
#'
#' @import ggplot2
#' @import showtext
#' @import ggtext
#'
#' @examples
#' \ village {
#' library(ggplot2)
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point() +
#'   labs(title = "Tytuł wykresu", subtitle = "Podtytuł", caption = "Źródło: NASK") +
#'   theme_nask()
#' }
#' @export

theme_nask <- function(font_style = 'thin') {
  require(showtext)
  require(ggtext)
  # require(systemfonts)
  #
  # sys_fonts <- systemfonts::system_fonts()[, c("path", "index", "family", "style", 'weight')]
  #
  # if(nrow(
  #   sys_fonts %>%
  #   filter(family == 'Archivo')
  # ) != 0) {
  #
  #   archivo_path <- sys_fonts %>%
  #     filter(family == 'Archivo' & weight == font_style) %>%
  #     pull(path)
  #
  #   font_add("Archivo",
  #            archivo_path)
  #
  #   showtext_auto()
  #
  #   font_family = 'Archivo'
  # }
  #
  # if(nrow(
  #   sys_fonts %>%
  #   filter(family == 'Archivo')
  # ) == 0) {
  #
  #   font_family = 'Arial'
  # }

  font_family = 'Arial'

  # font_add_google(name = 'Archivo',
  #                 family = "archivo",
  #                 bold.wt = 600,
  #                 db_cache = TRUE)


  # Set base theme and font family =============================================
  theme_minimal(
    base_family = font_family,
    base_size = 12,
    ink = kolory_techniczne['czarny']
  ) +
    # Overwrite base theme defaults ============================================
  theme(
    # Text elements ==========================================================
    plot.title = element_text(
      size = 18,
      face = "bold",
      color = "black",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 16,
      face = "bold",
      color = "black",
      margin = margin(b = 10)
    ),
    plot.caption = element_text(
      size = 11,
      face = 'plain',
      color = "#757576",
      margin = margin(t = 15),
      hjust = 0
    ),
    axis.text = element_text(
      size = 12,
      face = 'plain',
      color = "#9D9D9D"
    ),
    axis.title = element_text(
      size = 12,
      face = 'plain',
      color = '#757576'
    ),
    strip.text.x = element_markdown(hjust = 0.5,
                                    vjust = 1,
                                    size = 14),
    # Text elements ----------------------------
    plot.title.position = "plot",,
    plot.caption.position = "plot",

    # facets --------

    panel.grid = element_blank(),

    # Line elements ==========================================================
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      linetype = "dashed",
      linewidth = 0.15,
      color = "#999999"
    ),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(
      linetype = "solid",
      linewidth = 0.25,
      color = "#999999"
    ),
    axis.ticks.length.x = unit(4, units = "pt"),
    axis.line = element_line(color = '#9D9D9D',
                             linewidth = 0.3)
  )

}
