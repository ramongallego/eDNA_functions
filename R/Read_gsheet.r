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


## Normal PCRs
## Capture the conditions and samples

read_step1_PCR <- function(ss, trim = T, name = T){
  
  require(googlesheets4)
  require(tidyverse)
  
  # This works with my PCR spreadsheets.
  
  # Capture PCR conditions
  
  PCR_mix <- read_sheet(ss = ss,
                        range = cell_limits(ul = c(3, 1),
                                            lr = c(12, 7)),
                        col_names = F, 
                        col_types = "ccccccd") %>% 
    select(1,7) %>% 
    rename(Reagent = 1, Volume = 2)
  
  #return(PCR_mix)
  
  Cycling_conditions <- read_sheet(ss = ss,
                                   range = cell_limits(ul = c(1, 26),
                                                       lr = c(7, 32)),
                                   col_names = T, 
                                   col_types = "ccccddd") %>% 
    select(Step, temp, time_secs)  
   
 # return(Cycling_conditions)
  # 
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
    select(Well, Sample, Success, Notes) -> Samples
  
  if(trim){Samples <- Samples %>% filter (!is.na(Sample))}
  
  if(name){x<- gs4_get(ss)$name
  Samples$PCR <-  x}
  
  return(list(PCR_mix = PCR_mix,
              Cycling = Cycling_conditions,
              Samples = Samples))
  
  
}

# read_quick_PCR <- function(ss, trim = T, name = T){
#   
#   require(googlesheets4)
#   require(tidyverse)
#   
#   # This works with my PCR spreadsheets.
#   
#   # Capture PCR conditions
#   
#   PCR_mix <- read_sheet(ss = ss,
#                         range = cell_limits(ul = c(3, 1),
#                                             lr = c(12, 7)),
#                         col_names = F, 
#                         col_types = "ccccccd") %>% 
#     select(1,7) %>% 
#     rename(Reagent = 1, Volume = 2)
#   
#   #return(PCR_mix)
#   
#   Cycling_conditions <- read_sheet(ss = ss,
#                                    range = cell_limits(ul = c(1, 26),
#                                                        lr = c(7, 32)),
#                                    col_names = T, 
#                                    col_types = "ccccddd") %>% 
#     select(Step, temp, time_secs)  
#   
#   ## Copy everything to a temp file
#   
#   temp.sheet <- read_sheet(ss = ss,
#                            range = cell_limits(ul = c(16, 1),
#                                                lr = c(44, 36)),
#                            col_names = F,
#                            col_types = "c")
#   file <- tempfile(fileext = ".csv")
#   
#   write_csv(temp.sheet, col_names = F, file = file)
#   
#   
#   # return(Cycling_conditions)
#   # 
#   Sets <- list (c(1,9), c(11, 19), c(21,29))
#   width <- 6
#   times <- 6
#   
#   # Create a set of limits
#   limits <- map(1:times, function(.x){
#     list (start = (1+((.x -1)*width)),
#           finish = width + ((.x -1)* width) )
#     
#     
#   })
#   
#   # use them with the sets
#   
#   map(Sets, function(.y){
#     
#     
#     map(limits, function(.x){
#       
#       read_lid(file  = file,
#                  range = cell_limits(ul = c(.y[1], .x$start),
#                                      lr = c(.y[2], .x$finish)),
#                  col_names = T,
#                  col_types = "c")
#       
#     }
#     )
#     
#   }) %>% bind_rows()  %>%
#     select(Well, Sample, Success, Notes) -> Samples
#   
#   if(trim){Samples <- Samples %>% filter (!is.na(Sample))}
#   
#   if(name){x<- gs4_get(ss)$name
#   Samples$PCR <-  x}
#   
#   return(list(PCR_mix = PCR_mix,
#               Cycling = Cycling_conditions,
#               Samples = Samples))
#   
#   
# }
# 
