#' Create scaled relative proportions of the number of reads of taxa in different samples
#'
#' This function takes a long ASV table (tibble) and creates a new column with the relative proportions scaled to their maximum,
#' to avoid the dominance of species with better primer efficiency
#'
#' @param tibble A tibble the ASV table in a long format, with at least three columns, Sample_column, OTU_column, Counts_column
#' @param Sample_column The column indicating the sample.
#' @param OTU_column The column indicating the OTU/ASV.
#' @param Counts_column The column (numeric) with the number of sequences from that OTU in that sample.
#' @param Biological_replicate_column The column representing replicate measurements of Sample_column. 
#' @param ... Any extra columns that want to be added to the final dataset (either taxonomical information about OTUs, or metadata information about the samples)
#' 
#' @importFrom dplyr group_by summarise ungroup select mutate left_join across first everything
#' @importFrom rlang enquo quo_name
#' 
#' @return A tibble with at least the columns Sample_column, OTU_column and Normalized.reads
#' @export
#'
#' @examples
#' \dontrun{
#' data("training.ASV.table")
#' eDNAindex(training.ASV.table, Sample_column = Sample_name)
#' }


eDNAindex<- function(tibble,
                     Sample_column=Sample,
                     OTU_column=Hash,
                     Counts_column=nReads,
                     Biological_replicate_column=NULL,
                     ... ) { 

  Sample_column  <- enquo(Sample_column)
  OTU_column     <- enquo(OTU_column)
  Counts_column  <- enquo(Counts_column)
  Biological_replicate_column         <- enquo(Biological_replicate_column)
  # Keep metadata / extra columns
  tibble |>  
    ungroup() |>  
    select (!!Sample_column, !!OTU_column, ...) |>  
    group_by(!!Sample_column, !!OTU_column) |>  
    summarise(across(everything(), first), .groups = "drop") -> matching.df
  
  
  if (!is.null(Biological_replicate_column) &&
      quo_name(Biological_replicate_column) %in% colnames(tibble)) {
    
    message ("Averaging ratios between Biological replicates")
    
    tibble |>  
      
      group_by(!!Sample_column, !!OTU_column, !!Biological_replicate_column) |> 
      
      summarise ( sumreads = sum(!!Counts_column)) |>   # This sums technical replicates
      
      group_by(!!Sample_column,!!Biological_replicate_column) |>  
      
      mutate (Tot = sum(sumreads),
              Row.prop = sumreads / Tot)  |>                       # This creates the proportion on each biological replicate    
      
      group_by(!!Sample_column) |>  
      
      mutate (nreps = length(unique(!!Biological_replicate_column))) |>  
      
      group_by(!!Sample_column, !!OTU_column) |>  
      
      summarise (mean.prop = sum (Row.prop) / max(nreps))   |>  
      
      group_by (!!OTU_column) |> 
      
      mutate (Colmax = max (mean.prop),
              Normalized.reads = mean.prop / Colmax) |>  
      
      select( -Colmax, -mean.prop) -> output 
   
  } else {
  
  
  message ("Calculating eDNAindex directly")

  
  tibble |>  
    
    group_by(!!Sample_column, !!OTU_column) |> 
    
    summarise (sumreads = sum(!!Counts_column)) |>  # In case the sample column is of a higher group than the nrows
    
    group_by(!!Sample_column) |>  
    
    mutate (Tot = sum(sumreads),
            Row.prop = sumreads / Tot) |>  
    
    group_by (!!OTU_column) |> 
    
    mutate (Colmax = max (Row.prop),
            Normalized.reads = Row.prop / Colmax) |>  
    select(-Tot, -Row.prop, -Colmax, -sumreads) -> output 
  }
  

  left_join(output, matching.df) -> output
 
  return (output)
}
