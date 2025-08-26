#' Read Indexing PCR Spreadsheet
#'
#' Reads data from a Google Sheets-based indexing PCR template. 
#' You can access the template 
#' \href{https://docs.google.com/spreadsheets/d/1JdnvURK12fPN4lcm3HcVDPQ3kiL6TsOszLzgSx0c_Tw/edit?usp=sharing}{here},
#' create a copy in your Google Drive, and then create copies for each of your multiplexing PCR experiments.  
#'
#' The function extracts sample information across all plates and returns a tidy dataframe.
#'
#' @param ss Google Sheet ID or URL of the indexing PCR spreadsheet.
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{Well}{Well position}
#'   \item{Sample}{Sample name}
#'   \item{Barcode}{Barcode assigned to the sample}
#'   \item{Set}{Barcode set identifier}
#' }
#'
#' @examples
#' \dontrun{
#' read_indexing_PCR("https://docs.google.com/spreadsheets/d/...")
#' }
#'
#' @seealso [read_step1_PCR()] for reading normal PCR spreadsheets.
#'
#' @importFrom googlesheets4 read_sheet cell_limits gs4_get
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select rename filter
#' 
#' @rdname read_PCR
#' @export
read_indexing_PCR <- function (ss) {
  Sets <- list(c(16,24), c(26,34), c(36,44))
  width <- 6
  times <- 6
  
  limits <- map(1:times, function(.x){
    list(start = (1+((.x-1)*width)),
         finish = width + ((.x-1)*width))
  })
  
  map(Sets, function(.y){
    map(limits, function(.x){
      read_sheet(ss = ss,
                 range = cell_limits(ul = c(.y[1], .x$start),
                                     lr = c(.y[2], .x$finish)),
                 col_names = TRUE,
                 col_types = "c")
    })
  })  |> 
    bind_rows()  |> 
    select(Well, Sample, Barcode, Set)
}
#' Read Normal PCR Spreadsheet
#'
#' Reads data from a Google Sheets-based PCR template.  
#' You can access the template 
#' \href{https://docs.google.com/spreadsheets/d/1LBsuUBL2Jx83zz3yIZ2LYGFEmzx0jtPP0T_oG2XW5CQ/edit?usp=sharing}{here}, 
#' create your own copy, and then create one copy per PCR reaction you want to keep track of.
#'
#' Captures reagent mix, cycling conditions, and sample information.
#'
#' @param ss Google Sheet ID or URL of the indexing PCR spreadsheet.
#' @param trim Logical. If TRUE, removes rows where `Sample` is NA (default: TRUE).
#' @param name Logical. If TRUE, adds the spreadsheet name as a `PCR` column in the sample sheet (default: TRUE).
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{PCR_mix}{Tibble of reagents and volumes.}
#'   \item{Cycling}{Tibble of PCR cycling conditions.}
#'   \item{Samples}{Tibble of samples with columns `Well`, `Sample`, `Success`, `Notes`, and optionally `PCR`.}
#' }
#'
#' @examples
#' \dontrun{
#' read_step1_PCR("https://docs.google.com/spreadsheets/d/...")
#' }
#'
#' @seealso [read_indexing_PCR()] for reading multiplexing PCR spreadsheets.
#'
#' @rdname read_PCR
#' @export
read_step1_PCR <- function(ss, trim = TRUE, name = TRUE) {
  PCR_mix <- read_sheet(ss = ss,
                        range = cell_limits(ul = c(3,1),
                                            lr = c(13,7)),
                        col_names = FALSE,
                        col_types = "ccccccd")  |> 
    select(1,7) |> 
    rename(Reagent = 1, Volume = 2)
  
  Cycling_conditions <- read_sheet(ss = ss,
                                   range = cell_limits(ul = c(1,26),
                                                       lr = c(7,32)),
                                   col_names = TRUE,
                                   col_types = "ccccddd") |> 
    select(Step, temp, time_secs)
  
  Sets <- list(c(16,24), c(26,34), c(36,44))
  width <- 6
  times <- 6
  
  limits <- map(1:times, function(.x){
    list(start = (1+((.x-1)*width)),
         finish = width + ((.x-1)*width))
  })
  
  Samples <- map(Sets, function(.y){
    map(limits, function(.x){
      read_sheet(ss = ss,
                 range = cell_limits(ul = c(.y[1], .x$start),
                                     lr = c(.y[2], .x$finish)),
                 col_names = TRUE,
                 col_types = "c")
    })
  })  |> 
    bind_rows()  |> 
    select(Well, Sample, Success, Notes)
  
  if (trim) Samples <- Samples |> filter(!is.na(Sample))
  if (name) Samples$PCR <- gs4_get(ss)$name
  
  return(list(PCR_mix = PCR_mix,
              Cycling = Cycling_conditions,
              Samples = Samples))
}
