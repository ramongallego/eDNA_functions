#' Create contingency tables with two variables
#'
#' This function takes a tibble and create human readable 
#' contingency tables from two variables, either by showing 
#' number of cases in each combination or weighted by the sum of a numerical variable
#'
#' @param tibble A tibble containing at least two columns
#' @param rows The column with the levels included as rows in the final table.
#' @param cols The column with the levels included as columns in the final table.
#' @param wt The column (numeric) whose values to add in order to fill the cells. If wt = NULL (the default), counts are returned instead of weighted sums.
#' @param ... Any parameters that can be passed to tally_wide 'values_fill' is a useful one
#' 
#' @importFrom dplyr group_by tally
#' @importFrom tidyr pivot_wider
#' @importFrom rlang enquo
#' 
#' @return A tibble
#' @export
#'
#' @examples
#' data("training.metadata")
#' tally_wide(training.metadata, rows= Transect, cols = position)
tally_wide <- function (tibble, rows, cols, wt = NULL,...){
  
  rows = enquo(rows)
  cols = enquo(cols)
  weight = enquo(wt)
  
  
  tibble |> 
    group_by(!!rows, !!cols) |>  
    tally (wt = !!weight) |>  
    pivot_wider(names_from=  !!cols,
                values_from = n, names_repair = "minimal",...)
  
}

