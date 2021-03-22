# A function to read my indexing PCRs template

read_indexing_PCR <- function (ss){
  
  require(googlesheets4)
  require(tidyverse)
  
  Sets <- list (c(16,24), c(26, 34), c(36,44))
  width <- 6
  times <- 6
  
  # Create a set of limits
  limits <- map(1:times, function(.x){
    list (start = (1+((.x -1)*width)),
          finish = width + ((.x -1)* width) )
    
    
  })
  
  # use them with the sets
  
  map(Sets, function(.y){
    
    
    map(limits, function(.x){
      
      read_sheet(ss = ss,
                 range = cell_limits(ul = c(.y[1], .x$start),
                                     lr = c(.y[2], .x$finish)),
                 col_names = T, 
                 col_types = "c")
      
    }
    )
  }) %>% bind_rows() %>% 
    select(WELL, SAMPLE, BARCODE, Set)
  
}

