#' Title
#'
#' @param .data 
#' @param wzor_nazw_zmiennych 
#' @param zmienne 
#' @param ordered 
#' @param rev_values 
#' @param sort_value 
#' @param last_value 
#' @param items_labels 
#' @param items_labels_width 
#' @param values_text_cutoff 
#' @param digits 
#' @param fill_color_palette 
#' @param fill_color_direction 
#' @param fill_labels 
#' @param legend_n_row 
#' @param coord_flip 
#' @param title 
#' @param subtitle 
#' @param caption 
#'
#' @returns
#' @export
#'
#' @examples
wykres_bloku_pytan <- function(.data = data, 
                               wzor_nazw_zmiennych = '',
                               zmienne = NULL,
                               ordered = FALSE,
                               rev_values = FALSE,
                               sort_value = NULL, 
                               last_value = NULL,
                               items_labels = NULL,
                               items_labels_width = 60, 
                               values_text_cutoff = 5,
                               digits = 1,
                               fill_color_palette = 'Greys',
                               fill_color_direction = 1,
                               fill_labels = NULL,
                               legend_n_row = 2,
                               coord_flip = TRUE, 
                               title = '',
                               subtitle = '',
                               caption = ''
) { 
  require(dplyr)
  require(ggplot2)
  require(stringr)
  require(farver)
  require(paletteer)
  require(forcats)
  require(tidyr) # Dodane dla pivot_longer
  
  # --- Funkcja kontrastu ---
  contrast <- function(colour) {
    out   <- rep("black", length(colour))
    light <- farver::get_channel(colour, "l", space = "hcl")
    out[light < 50] <- "white"
    out
  }
  autocontrast <- aes(colour = after_scale(contrast(fill)))
  
  # --- Subsetting ---
  if(wzor_nazw_zmiennych != '') {
    df_sub <- .data %>% select(starts_with(wzor_nazw_zmiennych))
  } 
  if(!is.null(zmienne)){
    df_sub <- .data %>% select(all_of(zmienne))
  }
  
  # --- Przygotowanie danych ---
  df_sub <- df_sub %>% 
    pivot_longer(cols = everything()) %>% 
    filter(!is.na(value)) %>% 
    group_by(name) %>% 
    count(value, .drop = FALSE) %>% 
    mutate(proc = 100*(n/sum(n)),
           proc_cleaned = if_else(proc < values_text_cutoff, 
                                  true = '', 
                                  false = paste0(round(proc, digits = digits), '%')),
           totals = sum(n)) %>%
    # Obliczanie wartości do sortowania (dynamiczne)
    group_by(name) %>%
    mutate(proc_sort_value = if(!is.null(sort_value) && sort_value %in% value) 
      sum(proc[value == sort_value]) else 0) %>%
    ungroup()
  
  # Zawijanie długich etykiet wartości (legendy)
  if(max(nchar(as.character(df_sub$value)), na.rm = TRUE) > 16){
    df_sub <- df_sub %>%
      mutate(value = factor(value, labels = str_wrap(unique(value), width = 17)))
  }
  
  # --- Logika Sortowania (Kluczowy fragment) ---
  
  df_sub <- df_sub %>% ungroup()
  
  # 1. Fizyczne sortowanie wg wartości
  if(!is.null(sort_value)) {
    df_sub <- df_sub %>% arrange(
      #desc(
      proc_sort_value)
    #)
  }
  
  # 2. Ustalenie kolejności poziomów (faktoryzacja)
  df_sub <- df_sub %>% 
    mutate(name = factor(name, levels = unique(name)))
  
  # 3. Wyjątek: przesuwanie konkretnej zmiennej na koniec
  if(!is.null(last_value) && last_value %in% df_sub$name) {
    df_sub <- df_sub %>% 
      mutate(name = fct_relevel(name, last_value, after = 0))
  }
  
  # --- Wykres ---
  the_plot <- df_sub %>% 
    ggplot(aes(x = name, y = proc, fill = value)) +
    geom_col(color = 'gray90')
  
  # Etykiety osi X (pytania)
  if(!is.null(items_labels)) {
    the_plot <- the_plot + 
      scale_x_discrete(
        breaks = names(items_labels),
        labels = str_wrap(items_labels, width = items_labels_width)
      )
  }
  
  # Warstwy tekstowe
  the_plot <- the_plot +
    geom_text(aes(label = proc_cleaned, !!!autocontrast, group = value),
              position = position_stack(vjust = 0.5),
              size = 3, na.rm = TRUE, show.legend = FALSE) +
    geom_text(aes(y = 102, label = paste0('N=', round(totals, 0))), 
              size = 3, color = 'gray60', check_overlap = TRUE,
              hjust = 0) # Wyrównanie do lewej od punktu 105
  
  if(coord_flip) { the_plot <- the_plot + coord_flip() }
  
  
  if (length(fill_color_palette) > 1) {
    the_plot <- the_plot + scale_fill_manual(values = fill_color_palette, labels = fill_labels)
  } else {
    the_plot <- the_plot + scale_fill_brewer(type = 'seq', 
                                             palette = fill_color_palette, 
                                             direction = fill_color_direction,
                                             labels = fill_labels)
  }
  
  
  the_plot <- the_plot +
    #scale_fill_brewer(type = 'seq', palette = fill_color_palette, direction = fill_color_direction) +
    scale_colour_manual(values=c("#000000", "#ffffff"), guide = 'none') +
    labs(x = '', y = '%', title = title, subtitle = subtitle, caption = caption) +
    ylim(0, 110) +
    theme_minimal() +
    theme(legend.position = 'bottom',
          legend.title = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
  
  return(the_plot + guides(fill = guide_legend(nrow = legend_n_row)))
}
