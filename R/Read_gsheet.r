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

  }) %>% bind_rows()  %>% 
   select(Well, Sample, Barcode, Set)

  
}


write_indexing_PCR <- function (data, name, ss_template="1naS-F_dj4SNmND5nJ5TKhMX3TikmRS00ILKjfg_Ucgc" ){
  
  require(googlesheets4)
  require(tidyverse)
  
  ss_obj = gs4_create(name)
   
  sheet_copy(from_ss = ss_template, to_ss= ss_obj, to_sheet = "Good")
  
  sheet_delete(ss_obj, "Sheet1")
  
  data %>% 
    group_by(Column) %>% 
    group_split() %>% 
    map(~.x %>% select(Well, Sample) %>% 
          mutate(Barcode = Well,
                 B1 = "",
                 B2 = "",
                 Set = "B"))-> list.of.dfs
  
  
  Sets <- list (c(16,24), c(26, 34), c(36,44))
  width <- 6
  
  times <- 6
  
  
  
  
  # Create a set of limits
  limits <- map(1:times, function(.x){
    list (start = (1+((.x -1)*width)),
          finish = width + ((.x -1)* width) )
    
    
  })
  
  # use them with the sets
 tibble (Sets = rep(Sets, each = 6),
         Limits = rep(limits, 3)) %>% rownames_to_column("Pos") -> Positions
 
 tibble(list.of.dfs) %>% 
   rownames_to_column("Pos") -> datasets
 
 left_join(datasets, Positions) %>% 
   mutate(write = pwalk(.l = list (list.of.dfs, Sets, Limits),
                        .f = function(a, b, c){
                          range_write(ss_obj, 
                                      data = a,
                                      range = cell_limits(ul = c(b[1], c$start),
                                                          lr = c(b[2], c$finish)))
                          
                        } ))
  

  
}
