#' Skala wypełnienia NASK dla ggplot2
#'
#' Implementuje oficjalną paletę barw NASK dla estetyki \code{fill}. 
#' Automatycznie dobiera odpowiednią długość palety w zależności od liczby kategorii.
#'
#' @param type Typ palety: 
#' \itemize{
#'   \item \code{"nominal"} - jakościowa (8 lub 12 kolorów).
#'   \item \code{"unipolar"} - jednobiegunowa.
#'   \item \code{"bipolar"} - dwubiegunowa.
#'   \item \code{"diverging"} - rozbieżna.
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
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_nask(type = "nominal")
scale_fill_nask <- function(type = c("nominal", "unipolar", "bipolar", "diverging"), 
                            reverse = FALSE, ...) {
  
  type <- match.arg(type)
  
  pal_func <- function(n) {
    pal <- switch(type,
                  "nominal"   = if (n <= 8) kolory_nominalne_8 else kolory_nominalne_12,
                  "unipolar"  = if (n <= 4) kolory_unipolar_4 else if (n <= 5) kolory_unipolar_5 else if (n <= 7) kolory_unipolar_7 else kolory_unipolar_11,
                  "bipolar"   = if (n <= 4) kolory_bipolar_4 else if (n <= 5) kolory_bipolar_5 else if (n <= 7) kolory_bipolar_7 else kolory_bipolar_11,
                  "diverging" = if (n <= 4) kolory_diverging_4 else if (n <= 5) kolory_diverging_5 else if (n <= 7) kolory_diverging_7 else kolory_diverging_11
    )
    
    if (n > length(pal)) {
      warning(sprintf("Paleta '%s' ma max %d kolorów. Używasz %d.", type, length(pal), n))
    }
    
    out <- pal[1:min(n, length(pal))]
    if (reverse) out <- rev(out)
    return(out)
  }
  
  ggplot2::discrete_scale("fill", "nask", pal_func, ...)
}