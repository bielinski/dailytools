#'Wykres bloku pytań
#'
#'Funkcja worzy wykres przedstawiający rozkłady odpowiedzi na pytania z bloku
#'pytań kwestionariusza (np. pytania z oceną stwierdzeń). Zmienne przedstawiane
#'na wykresie powinny mieć takie same możliwe wartości
#'
#'@param .data ramka danych
#'@param wzor_nazw_zmiennych (opcjonalnie) wspólny element nazw zmiennych
#'  (string) pozwalający jednoznacznie zidentyfikować zmienne z bloku pytań
#'@param zmienne wektor nazw zmiennych
#'@param ordered (opcjonalnie) TRUE/FALSE czy zmienne są typu `ordered`
#'@param rev_values TRUE/FALSE czy odwrócić kolejność wartości zmiennej na
#'  wykresie
#'@param sort_value (string) wartość zmiennych, wg częstości której będa
#'  sortowane zmienne na wykresie
#'@param last_value (string) zmienna, która ma być na ostatnim miejscu na
#'  wykresie
#'@param items_labels wektor etykiet dla poszczególnych zmiennych
#'  `c(nazwa_zmiennej = 'etykieta')`
#'@param items_labels_width liczba znaków zawijania etykiet zmienych
#'@param values_text_cutoff wartość etykiet wartości poniżej której etykiety
#'  wartości nie sa wyświetlane na wykresie (domyślnie 5)
#'@param digits liczba miejsc po przecinku do zaokrąglenia etykiet wartości
#'@param fill_color_palette paleta dla wypełnień słupków
#'@param fill_color_direction kierunek układu kolorów na wykresie (1 lub -1 dla
#'  odróconej kolejności)
#'@param fill_labels wektor etykiet dla poszczególnych kolorów wypełnienia
#'@param legend_n_row liczba wierszy dla elementów legendy
#'@param coord_flip TRUE/FALSE czy odwrócić osie wykresu
#'@param title tytuł na wykresie
#'@param subtitle podtytuł na wykresie
#'@param caption informacja pod wykresem
#'
#'@returns obiekt ggplot2
#'@export
#'
#' @examples
#'
#' p16_labels <- c(
#'p16_1_fct ='ChatGPT',
#'p16_2_fct ='Copilot',
#'p16_3_fct ='Gemini',
#'p16_4_fct ='Cloude',
#'p16_5_fct ='Perplexity',
#'p16_6_fct ='DeepSeek',
#'p16_7_fct ='PLLuM',
#'p16_8_fct ='Bielik',
#'p16_9_fct ='Inne'
#')
#'
#'
#'wykres_bloku_pytan(.data = dane,
#'                   zmienne = paste0('p16_', 1:9, '_fct'),
#'                   rev_values = FALSE,
#'                   #fill_color_palette = niebieski_paleta[c(1, 9)]
#'                   legend_n_row = 1,
#'                   title = 'Narzędzia GenAI wykorzystywane w pracy przez pracowników #'administracji publicznej w Polsce',
#'                   subtitle = 'Z jakich narzędzi AI korzystał/a w ciągu ostatnich 6 miesięcy w #'pracy?',
#'                   caption = realizacja,
#'                   items_labels = p16_labels,
#'                   items_labels_width = 60,
#'                   sort_value = 'Zaznaczono',
#'                   last_value = 'p16_9_fct'
#')
#' 

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
