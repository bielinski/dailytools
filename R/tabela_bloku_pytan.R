#' Tabela bloku pytań
#'
#' `tabela_bloku_pytan()` tworzy tabelę z rozkładem zmiennych pochodzących z
#' bloku pytań w kwestionariuszu, w których wykorzystano te same odpowiedzi
#' (wartości zmiennych sa takie same).
#'
#' @param .data ramka danych
#' @param wzor_nazw_zmiennych (opcjonalnie) wspólny element nazw zmiennych
#'   (string) pozwalający jednoznacznie zidentyfikować zmienne z bloku pytań
#' @param zmienne wektor nazw zmiennych
#' @param tab_form określa kształt tabeli: `wide` (szeroka) lub `long` (długa)
#'
#' @returns ramke danych (tibble)
#' @export
#'
#' @examples
#'
#' tabela_bloku_pytan(dane,
#'                    zmienne = c('imd1_1_fct',
#'                                'imd1_2_fct',
#'                                'imd1_3_fct',
#'                                'imd1_4_fct',
#'                                'imd1_5_fct')
#'                    )
#'
#' tabela_bloku_pytan(.data = dane, zmienne = paste0('p16_', 1:9, '_fct'))
#'
#' tabela_bloku_pytan(.data = dane, wzor_nazw_zmiennych = 'p16_')
#' 

tabela_bloku_pytan <- function(.data = data, 
                               wzor_nazw_zmiennych = '',
                               zmienne = NULL,
                               tab_form = 'wide') {
  
  if(wzor_nazw_zmiennych != '') {
    df_sub <- .data %>%
      select(starts_with(wzor_nazw_zmiennych))
  } 
  
  if(!is.null(zmienne)){
    df_sub <-  .data %>% 
      select(all_of(zmienne))
  }
  df_sub <- df_sub %>% 
    pivot_longer(cols = everything()) %>% 
    filter(!is.na(value)) %>% 
    group_by(name) %>% 
    count(value) %>% 
    mutate(proc = 100*(n/sum(n))) 
  
  # df_sub_n <- df_sub %>% 
  # 	select(-proc) %>% 
  # 	pivot_wider(names_from = value, 
  # 				values_from = n) %>% 
  # 	ungroup()
  
  df_sub_n <- df_sub %>% 
    summarize(N = sum(n))
  
  df_sub_proc <- df_sub %>% 
    summarize(Razem = sum(proc))
  
  if(tab_form == 'wide'){
    df_sub_proc_table <- df_sub %>% 
      select(-n) %>% 
      pivot_wider(names_from = value,
                  values_from = proc) %>% 
      ungroup()
    
    return(bind_cols(df_sub_proc_table,
                     Razem = df_sub_proc$Razem, 
                     N = df_sub_n$N))
  }
  
  if(tab_form == 'long'){
    
    df_sub <-df_sub %>%
      mutate(
        value = str_trim(value)
      )
    
    return(df_sub)
  }
  
}