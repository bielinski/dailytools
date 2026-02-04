#' Wykres słupkowy
#'
#' @param .data ramka danych
#' @param column nazwa zmiennej
#' @param cross_var TODO: nazwa zmiennej grupującej dla facet_wrap()
#' @param weight waga analityczna
#' @param fill_color kolor wypelnienia słupków
#' @param title tytuł na wykresie
#' @param subtitle podtytuł na wykresie
#' @param caption informacja pod wykresem
#' @param expand_top wartość określająca o ile zwiększyć oś y
#' @param axix_x_labs_wrap liczba znaków do zawijania etykiet wartości zmiennej na osi x
#' @param co_flip TRUE/FALSE czy odwrócic osie wykresu
#' @param rev_values TRUE/FALSE czy odwrócić kolejność wartości zmiennej na wykresie
#' @param highlighted_cat wartośc zmiennej do wyróżnienia osobnym kolorem
#' @param highlight_fill kolor wyróżnienia osobnej wartości (jeśli `highlighted_cat` != NULL)
#' @param ... dodatkowe argumenty ggplot2
#'
#' @returns a ggplot2 object
#' @export
#'
#' @examples
#' 
#' mtcars$gear_fct <- as.factor(mtcars$gear)
#' wykres_slupkowy(.data = mtcars, column = gear_fct)
#' 
wykres_slupkowy <- function(.data, 
                            column,
                            cross_var = NULL, #TODO: wykorzystanie argumentu dla facet_wrap()
                            weight = NULL,
                            fill_color = 'steelblue',
                            title = NULL,
                            subtitle = NULL,
                            caption = NULL,
                            expand_top = 0.15,
                            axix_x_labs_wrap = 20,
                            co_flip = FALSE, # czy odwrócić wykres
                            rev_values = FALSE,
                            highlighted_cat = NULL,
                            highlight_fill = NULL,
                            ...) {
  require(dplyr)
  require(tidyr)
  require(ggplot2)
  require(stringr)
  
  
  if(is.null(cross_var)){
    n_valid <- .data %>% 
      drop_na({{column}}) %>% 
      count({{column}}, ...) %>% 
      #filter(!is.na(values)) %>% 
      summarize(n = round(sum(n), digits = 0))  
    
    df_tmp <- .data %>% 
      drop_na({{column}}) %>% 
      count({{column}}) %>% 
      mutate(prop = 100*(n/sum(n))) %>% 
      rename(value = {{column}})
    
  } else { #TODO: do poprawy ten kod
    n_valid <- .data %>% 
      select({{column}}, cross_var) %>% 
      drop_na() %>% 
      count({{column}}, cross_var) %>% 
      summarize(n = round(sum(n), digits = 0)) 
    
    df_tmp <- .data %>% 
      group_by(cross_var) %>% 
      drop_na({{column}}) %>% 
      count({{column}}) %>% 
      mutate(prop = 100*(n/sum(n))) %>% 
      rename(value = {{column}})
  }
  
  
  # 	print(n_valid)
  #  print(df_tmp)
  
  
  the_plot <- df_tmp %>% 
    ggplot(aes(x = value, y = prop))
  
  
  #mechanizm wyróżniania jednej z kategorii na wykresie
  if(!is.null(highlighted_cat)){
    the_plot <- the_plot +
      geom_col(aes(
        fill=factor(ifelse(value==highlighted_cat,
                           highlight_fill,
                           fill_color))
      ), 
      show.legend = FALSE) +
      scale_fill_manual(name = vars(value), values=c(granatowy_paleta[8],granatowy))
  } else {
    the_plot <- the_plot +
      geom_col(fill = fill_color)
  }
  
  
  if(rev_values == TRUE){
    the_plot <- the_plot +
      scale_x_discrete(
        limits = rev, #odwrócenie kolejności itemów na wykresie (oś pozioma)
        labels = function(x) 
          str_wrap(x,
                   width = axix_x_labs_wrap))
  } else {
    the_plot <- the_plot +
      scale_x_discrete(labels = function(x) 
        str_wrap(x,
                 width = axix_x_labs_wrap))
  }
  
  
  the_plot <- the_plot +
    scale_y_continuous(expand = expansion(mult = c(0, expand_top))) +
    labs(
      title = title,
      subtitle = subtitle,
      caption = paste0(caption, '\n','N=', n_valid)
    ) +
    ylab('%')+
    xlab('')
  
  # obsługa argumenty co_flip
  if(co_flip){
    the_plot <- the_plot + 
      coord_flip() +
      geom_text(
        aes(
          label = paste0(sprintf("%0.1f",
                                 round(prop, 
                                       digits = 1)),
                         '%')
        ),
        hjust=-0.2, size=3.5) #TODO obsłużyć zmianę tych parametrów dla coord_flip
  } else {
    the_plot <- the_plot + 
      geom_text(
        aes(
          label = paste0(sprintf("%0.1f",
                                 round(prop, 
                                       digits = 1)),
                         '%')
        ),
        vjust=-0.3, size=3.5) #TODO obsłużyć zmianę tych parametrów dla coord_flip
  }
  
  if(!is.null(cross_var)){ #TODO: do poprawy ten kod
    the_plot <- the_plot + 
      facet_wrap(vars(cross_var), ncol = 2, scales = 'free')  
  }
  
  
  the_plot <- the_plot +
    theme_minimal()
  
  if(!co_flip){
    the_plot <- the_plot +
      theme(
        axis.title.x = element_blank()
      )
  } 
  
  the_plot
}