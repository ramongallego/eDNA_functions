#' Write Indexing PCR Spreadsheet
#'
#' Creates a new Google Sheet for indexing PCRs from a template,
#' fills in the sample information, and writes it into the correct
#' ranges of the sheet.
#'
#' @param data A tibble or dataframe with at least the columns:
#'   \describe{
#'     \item{Well}{Well position}
#'     \item{Sample}{Sample identifier}
#'     \item{Column}{Plate column (used to split data across sheet sections)}
#'   }
#' @param name Character. Name for the new Google Sheet that will be created.
#' @param ss_template Character. ID of the template sheet to copy from
#'   (default: `"1naS-F_dj4SNmND5nJ5TKhMX3TikmRS00ILKjfg_Ucgc"`).
#'
#' @return A Google Sheet object (as returned by [googlesheets4::gs4_create()])
#'   with the data written into the correct plate layout.
#'
#' @details
#' This function:
#' 1. Creates a new Google Sheet with the given `name`.
#' 2. Copies the template sheet into it.
#' 3. Splits the input `data` by plate column.
#' 4. Writes each split dataframe into its designated range of the sheet.
#'
#' @examples
#' #Examples are not executed because the function requires identification in Google 
#' \dontrun{
#' my_data <- tibble::tibble(
#'  Well = c("A1","A2"),
#'   Sample = c("Sample1","Sample2"),
#'   Column = c(1,2)
#' )
#' write_indexing_PCR(my_data, "PCR_001")
#' }
#'
#' @importFrom googlesheets4 gs4_create sheet_copy sheet_delete range_write cell_limits
#' @importFrom dplyr group_by group_split select mutate left_join
#' @importFrom purrr map pwalk
#' @importFrom tibble tibble rownames_to_column
#' @export
write_indexing_PCR <- function (data,
                                name,
                                ss_template = "1naS-F_dj4SNmND5nJ5TKhMX3TikmRS00ILKjfg_Ucgc") {
  
  ss_obj <- gs4_create(name)
  
  sheet_copy(from_ss = ss_template, to_ss = ss_obj, to_sheet = "Good")
  sheet_delete(ss_obj, 1)
  
  data |> 
    group_by(Column) |> 
    group_split() |> 
    map(~ .x |> 
          select(Well, Sample) |> 
          mutate(Barcode = Well,
                 B1 = "",
                 B2 = "",
                 Set = "B")) -> list.of.dfs
  
  Sets <- list(c(16,24), c(26,34), c(36,44))
  width <- 6
  times <- 6
  
  limits <- map(1:times, function(.x){
    list(start = (1+((.x-1)*width)),
         finish = width + ((.x-1)*width))
  })
  
  tibble(Sets = rep(Sets, each = times),
         Limits = rep(limits, length(Sets))) |> 
    rownames_to_column("Pos") -> Positions
  
  tibble(list.of.dfs) |> 
    rownames_to_column("Pos") -> datasets
  
  left_join(datasets, Positions, by = "Pos") |> 
    mutate(write = pwalk(.l = list(list.of.dfs, Sets, Limits),
                         .f = function(a, b, c){
                           range_write(ss_obj,
                                       data = a,
                                       range = cell_limits(
                                         ul = c(b[1], c$start),
                                         lr = c(b[2], c$finish)))
                         } ))
  
  return(ss_obj)
}
