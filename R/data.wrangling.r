# Create contingency tables with two variables

tally_wide <- function (tibble, rows, cols, wt = NULL){
  
  rows = enquo(rows)
  cols = enquo(cols)
  weight = enquo(wt)
  
  
  tibble %>% 
    group_by(!!rows, !!cols) %>% 
    tally (.,wt = {{wt}}) %>% 
    pivot_wider(names_from=  !!cols,
                values_from = n, names_repair = "minimal")
  
}

