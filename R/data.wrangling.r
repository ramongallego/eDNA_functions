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

tibbleseqs <- function(fastafile){
  insect::readFASTA(fastafile) -> temp
  tibble (header = str_remove(names(temp),";$"),
          seq = insect::dna2char(temp))
}

fasta_to_ASV_table <- function(path_to_fastas){
  
  map(path_to_fastas, tibbleseqs) |> 
    set_names(basename(path_to_fastas)) |> 
    bind_rows(.id="sample_name") |> 
    separate(header, into = c("Hash", "nReads"), sep = ";size=", convert=T)
    
  
}