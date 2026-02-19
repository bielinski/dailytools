#' Skala koloru NASK dla ggplot2
#'
#' Implementuje oficjalną paletę barw NASK dla estetyki \code{color} / \code{colour}. 
#' Automatycznie dobiera odpowiednią długość palety w zależności od liczby kategorii.
#'
#' @param type Typ palety: 
#' \itemize{
#'   \item \code{"nominal"} - jakościowa (8 lub 12 kolorów).
#'   \item \code{"unipolar"} - jednobiegunowa.
#'   \item \code{"bipolar"} - dwubiegunowa.
#'   \item \code{"diverging"} - rozbieżne.
#' }
#' @param reverse Logiczne. Czy odwrócić kolejność kolorów? Domyślnie \code{FALSE}.
#' @param ... Dodatkowe argumenty przekazywane do \code{ggplot2::discrete_scale}.
#'
#' @return Funkcja skali ggplot2.
#' @importFrom ggplot2 discrete_scale
#' @export
#'
#' @examples
#' library(ggplot2)
#' ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
#'   geom_point() +
#'   scale_color_nask(type = "nominal")
scale_color_nask <- function(type = c("nominal", "unipolar", "bipolar", "diverging"), 
                             reverse = FALSE, ...) {
  
  # Wykorzystujemy logikę z funkcji fill, aby zachować spójność
  sc <- scale_fill_nask(type = type, reverse = reverse, ...)
  sc$aesthetics <- "colour"
  return(sc)
}