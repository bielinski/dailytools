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