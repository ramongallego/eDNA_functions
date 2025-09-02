#' Internal: Tidy PCR sample blocks
#'
#' Helper that takes a messy dataframe with repeated blocks of 6 columns
#' and converts them into a single tidy tibble with expected columns.
#'
#' @param df A tibble as read from a Google Sheet.
#' @param keep Which columns to keep/rename. Default is for normal PCR sheets.
#'   Should be a named character vector like 
#'   `c("Well"=1, "Sample"=2, "Success"=5, "Notes"=6)`.
#' @param block_size Number of columns per block (default = 6).
#'
#' @return A tidy tibble.
#' @keywords internal
.tidy_PCR_blocks <- function(df,
                             keep = c("Well"=1, "Sample"=2, "Success"=5, "Notes"=6),
                             block_size = 6) {
  n_blocks <- ncol(df) / block_size
  
  blocks <- map(1:n_blocks, function(i) {
    start <- (i - 1) * block_size + 1
    end   <- i * block_size
    
    df[, start:end] |>
      select(!!!setNames(as.list(keep), names(keep))) |>
      filter(.data[[names(keep)[1]]] != names(keep)[1]) |> # drop header rows
      filter(!is.na(.data[["Sample"]])) |>                # drop NA Sample
      filter(.data[["Sample"]] != "")                     # drop empty Sample
  })
  
  bind_rows(blocks)
}

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
#' @importFrom googlesheets4 read_sheet cell_limits
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select filter
#' @importFrom rlang .data
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
  
  raw <- map(Sets, function(.y){
    map(limits, function(.x){
      read_sheet(ss = ss,
                 range = cell_limits(ul = c(.y[1], .x$start),
                                     lr = c(.y[2], .x$finish)),
                 col_names = TRUE,
                 col_types = "c")
    })
  }) |> bind_rows()
  
  .tidy_PCR_blocks(raw,
                   keep = c("Well"=1, "Sample"=2, "Barcode"=3, "Set"=4),
                   block_size = width)
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
#' @param ss Google Sheet ID or URL of the PCR spreadsheet.
#' @param trim Logical. If TRUE, removes rows where `Sample` is NA (default: TRUE).
#' @param name Logical. If TRUE, adds the spreadsheet name as a `PCR` column in the sample sheet (default: TRUE).
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{PCR_mix}{tibble of reagents and volumes.}
#'   \item{Cycling}{tibble of PCR cycling conditions.}
#'   \item{Samples}{tibble of samples with columns `Well`, `Sample`, `Success`, `Notes`, and optionally `PCR`.}
#' }
#'
#' @examples
#' \dontrun{
#' read_step1_PCR("https://docs.google.com/spreadsheets/d/...")
#' }
#'
#' @seealso [read_indexing_PCR()] for reading multiplexing PCR spreadsheets.
#'
#' @importFrom googlesheets4 read_sheet cell_limits gs4_get
#' @importFrom purrr map
#' @importFrom dplyr bind_rows select rename filter
#' @importFrom rlang .data
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
  
  raw <- map(Sets, function(.y){
    map(limits, function(.x){
      read_sheet(ss = ss,
                 range = cell_limits(ul = c(.y[1], .x$start),
                                     lr = c(.y[2], .x$finish)),
                 col_names = TRUE,
                 col_types = "c")
    })
  }) |> bind_rows()
  
  Samples <- .tidy_PCR_blocks(raw,
                              keep = c("Well"=1, "Sample"=2, "Success"=5, "Notes"=6),
                              block_size = width)
  
  if (trim) Samples <- filter(Samples, !is.na(.data$Sample))
  if (name) Samples$PCR <- gs4_get(ss)$name
  
  return(list(PCR_mix = PCR_mix,
              Cycling = Cycling_conditions,
              Samples = Samples))
}
