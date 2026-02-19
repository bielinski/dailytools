#' Palety kolorów zgodne z identyfikacją wizualną NASK
#'
#' Zbiór zdefiniowanych kolorów technicznych i marketingowych wykorzystywanych
#' w raportach oraz motywie \code{theme_nask}.
#'
#' @format Nazwane wektory tekstowe (character vector) zawierające kody HEX:
#' \describe{
#'   \item{kolory_nominalne_8}{paleta kolorów dla zmiennych nominalnych: 8 kolorów}
#'   \item{kolory_nominalne_12}{paleta kolorów dla zmiennych nominalnych: 12 kolorów}
#'   \item{kolory_unipolar_4}{paleta kolorów dla zmiennych porządkowych-unipolarnych: 4 kolory}
#'   \item{kolory_unipolar_5}{paleta kolorów dla zmiennych porządkowych-unipolarnych: 5 kolorów}
#'   \item{kolory_unipolar_7}{paleta kolorów dla zmiennych porządkowych-unipolarnych: 7 kolorów}
#'   \item{kolory_unipolar_11}{paleta kolorów dla zmiennych porządkowych-unipolarnych: 11 kolorów}
#'   \item{kolory_gradient}{kolor początkowy i końcowy gradientu dla zmiennych ciągłych}
#'   \item{kolory_bipolar_4}{paleta kolorów dla zmiennych porządkowych-bipolarnych: 4 kolory}
#'   \item{kolory_bipolar_5}{paleta kolorów dla zmiennych porządkowych-bipolarnych: 5 kolorów}
#'   \item{kolory_bipolar_7}{paleta kolorów dla zmiennych porządkowych-bipolarnych: 7 kolorów}
#'   \item{kolory_bipolar_11}{paleta kolorów dla zmiennych porządkowych-bipolarnych: 11 kolorów}
#'   \item{kolor_NA}{kolor dla wartości NA}
#'   \item{kolory_highlight}{dwa kolory analityczne dla wyróżniania elementów wykresu}
#'   \item{kolory_techniczne}{kolory techniczne dla elementów takich jak osie, tytuły etc.}
#'   
#' }
#' 
#' @name kolory_nask
#' @source Własne opracowanie na podstawie księgi znaku NASK i podręcznika BAiB 1.2
#' @examples
#' library(ggplot2)
#' 
#' # 1. Użycie palety nominalnej w barplocie
#' ggplot(mtcars, aes(x = factor(cyl), fill = factor(cyl))) +
#'   geom_bar() +
#'   scale_fill_manual(values = kolory_nominalne_8) +
#'   theme_nask()
#'
#' # 2. Użycie skali bipolarnej (np. dla korelacji lub skali Likerta)
#' df <- data.frame(x = 1:5, y = 1:5, z = c(-2, -1, 0, 1, 2))
#' ggplot(df, aes(x, y, color = z)) +
#'   geom_point(size = 10) +
#'   scale_color_gradientn(colors = kolory_bipolar_5) +
#'   theme_nask()
#'
#' # 3. Dostęp do kolorów technicznych
#' ggplot(mtcars, aes(mpg, wt)) +
#'   geom_point(color = kolory_techniczne['czarny']) +
#'   theme_nask()
#'   
NULL

# kolory nominalne ------

#' @rdname kolory_nask
#' @export
kolory_nominalne_8 <- c(
  '#447ABC', # 1
  '#739DDA', # 2
  '#1F3D7F', # 3
  '#7188AA', # 4
  '#275393', # 5
  '#495F75', # 6
  '#A0B0C1', # 7
  '#042C69'  # 8
)

#' @rdname kolory_nask
#' @export
kolory_nominalne_12 <- c(
  '#818EA5', # 1 
  '#447ABC', # 2 
  '#042C69', # 3 
  '#739DDA', # 4 
  '#7D8AA2', # 5 
  '#1F3D7F', # 6 
  '#7188AA', # 7 
  '#275393', # 8 
  '#758CAD', # 9 
  '#495F75', # 10
  '#A0B0C1', # 11
  '#0D2144'  # 12
)

# kolory skale unipolarne -------

#' @rdname kolory_nask
#' @export
kolory_unipolar_4 <- c(
  '#E6EEF9', # 1
  '#9FB8DA', # 2
  '#2E5792', # 3
  '#042C69'  # 4
)

#' @rdname kolory_nask
#' @export
kolory_unipolar_5 <- c(
  '#E6EEF9', # 1
  '#9FB8DA', # 2
  '#5881BB', # 3
  '#2E5792', # 4
  '#042C69'  # 5
)

#' @rdname kolory_nask
#' @export
kolory_unipolar_7 <- c(
  '#E6EEF9', # 1
  '#B7CAE4', # 2
  '#87A5D0', # 3
  '#5881BB', # 4
  '#3C65A0', # 5
  '#204884', # 6
  '#042C69'  # 7
)

#' @rdname kolory_nask
#' @export
kolory_unipolar_11 <- c(
  '#E6EEF9', # 1 
  '#CAD8EC', # 2 
  '#ADC2E0', # 3 
  '#91ADD3', # 4 
  '#7597C7', # 5 
  '#5881BB', # 6 
  '#3C6BAE', # 7 
  '#2E5B9D', # 8 
  '#204C8C', # 9 
  '#123C7A', # 10
  '#042C69'  # 11
)

# kolory skale ciągłe -------

#' @rdname kolory_nask
#' @export
kolory_gradient <- c(
  'low'  = '#E6EEF9', # 1
  'high' = '#042C69'  # 2
)

# kolory skale bipolarne -----

#' @rdname kolory_nask
#' @export
kolory_bipolar_4 <- c(
  '#73400A', # 1
  '#C29867', # 2
  '#91ADD3', # 3
  '#234E82'  # 4
)

#' @rdname kolory_nask
#' @export
kolory_bipolar_5 <- c(
  '#73400A', # 1
  '#C29867', # 2
  '#E6EEF9', # 3
  '#91ADD3', # 4
  '#234E82'  # 5
)

#' @rdname kolory_nask
#' @export
kolory_bipolar_7 <- c(
  '#73400A', # 1
  '#A87136', # 2 
  '#C29867', # 3 
  '#E6EEF9', # 4 
  '#91ADD3', # 5 
  '#5881BB', # 6 
  '#234E82'  # 7 
)

#' @rdname kolory_nask
#' @export
kolory_bipolar_11 <- c(
  '#73400A', # 1
  '#7F4C19', # 2
  '#A87136', # 3
  '#C29867', # 4
  '#DAB17B', # 5
  '#E6EEF9', # 6
  '#ADC2E0', # 7
  '#91ADD3', # 8
  '#5881BB', # 9
  '#2E5B9D', # 10
  '#123C7A'  # 11
)

# kolory skala rozbieżna (diverging) -------

#' @rdname kolory_nask
#' @export
kolory_diverging_4 <- c(
  '#353F47', # 1
  '#7B8794', # 2
  '#91ADD3', # 3
  '#123C7A'  # 4
)

#' @rdname kolory_nask
#' @export
kolory_diverging_5 <- c(
  '#353F47', # 1
  '#7B8794', # 2
  '#E6EEF9', # 3
  '#91ADD3', # 4
  '#123C7A'  # 5
)

#' @rdname kolory_nask
#' @export
kolory_diverging_7 <- c(
  '#353F47', # 1
  '#5B6872', # 2
  '#B1BAC5', # 3
  '#E6EEF9', # 4
  '#ADC2E0', # 5
  '#5881BB', # 6
  '#123C7A'  # 7
)

#' @rdname kolory_nask
#' @export
kolory_diverging_11 <- c(
  '#353F47', # 1
  '#434F59', # 2
  '#5B6872', # 3
  '#7B8794', # 4
  '#B1BAC5', # 5
  '#E6EEF9', # 6
  '#ADC2E0', # 7
  '#91ADD3', # 8
  '#5881BB', # 9
  '#2E5B9D', # 10
  '#123C7A'  # 11
)

# Pozostałe kolory -----

#' @rdname kolory_nask
#' @export
kolor_NA <- '#D9D9D9'

#' @rdname kolory_nask
#' @export
kolory_highlight <- list(
  pomarancz = '#EE7D50', # 1
  fiolet    = '#7359A1'  # 2
)

#' @rdname kolory_nask
#' @export
kolory_techniczne <- c(
  czarny       = '#000000', # 1
  ciemno_szary = '#757576', # 2
  szary        = '#9D9D9D'  # 3
)