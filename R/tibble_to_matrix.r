#' Convert a long tibble to a community matrix
#'
#' Converts a long-format table of sequence counts into a wide community
#' matrix (samples × taxa) suitable for vegan or other community ecology tools.
#'
#' @param long.table A tibble with at least sample, taxon, and abundance columns.
#' @param taxon Column containing taxa/OTU IDs (unquoted).
#' @param Abundance Column with counts/abundance values (unquoted).
#' @param sample.name Column with sample IDs (unquoted).
#'
#' @importFrom rlang enquo !!
#' @importFrom tidyr replace_na pivot_wider
#' @importFrom dplyr mutate select pull arrange if_else distinct
#' @importFrom tidyselect all_of
#' @importFrom vegan decostand vegdist
#' 
#' @return A numeric matrix with taxa as columns and samples as row names.
#' @export
#' @rdname tibble_to_vegan
#'
#' @examples
#' data("ASV_table")
#'  tibble_to_comm(ASV_table, taxon = Hash, Abundance = nReads, sample.name = sample_name)

tibble_to_comm <- function(long.table, taxon, Abundance, sample.name) {
  sample.name <- rlang::enquo(sample.name)
  taxon <- rlang::enquo(taxon)
  Abundance <- rlang::enquo(Abundance)
  
  long.table <- long.table |> 
    mutate(!!taxon := tidyr::replace_na(as.character(!!taxon), "NA"))
  
  
  taxa <- long.table |>  pull(!!taxon) |>  unique()
  
  matrix_1 <- long.table |> 
    select(!!sample.name, !!taxon, !!Abundance) |> 
    tidyr::pivot_wider(names_from = !!taxon,
                       values_from = !!Abundance,
                       values_fill = 0)
  
  samples <- pull(matrix_1, !!sample.name)
  
  matrix_1 <- select(matrix_1, all_of(taxa)) |>  data.matrix()
  rownames(matrix_1) <- samples
  
  return(matrix_1)
}



#' Convert long tibble to distance matrix
#'
#' Produces a distance matrix between samples using vegan's \code{vegdist()}.
#' Optionally applies a data transformation before distance calculation.
#'
#' @param long.table A tibble with sample, taxon, and abundance columns.
#' @param taxon Column containing taxa/OTU IDs (unquoted).
#' @param Abundance Column with counts/abundance values (unquoted).
#' @param sample.name Column with sample IDs (unquoted).
#' @param distance Distance metric to use (default = "bray").
#' @param transformation Optional transformation (e.g. "hellinger", "log"). See vegan::decostand documentation for a great explanation of all transformations
#' @param ... Extra metadata columns to keep alongside.
#'
#' @return A \code{dist} object.
#' @export
#' @rdname tibble_to_vegan
#' 
#' @examples
#' data("ASV_table")
#' tibble_to_dist(ASV_table, taxon = Hash, Abundance = nReads, sample.name = sample_name,distance = "bray", transformation = "hellinger")
#' 


tibble_to_dist <- function(long.table, taxon, Abundance, sample.name,
                           distance = "bray", transformation = NULL, ...) {
  
  sample.name <- rlang::enquo(sample.name)
  taxon <- rlang::enquo(taxon)
  Abundance <- rlang::enquo(Abundance)
  METHODS <- c("total", "max", "frequency", "normalize", "range", 
               "rank", "rrank", "standardize", "pa", "chi.square", "hellinger", 
               "log", "clr", "rclr", "alr")
  
  long.table <- long.table |> 
    mutate(!!taxon := if_else(is.na(!!taxon), "NA", as.character(!!taxon)))
  
  taxa <- long.table |>  pull(!!taxon) |>  unique()
  
  wide_tibble <- long.table |> 
    select(!!sample.name, !!taxon, !!Abundance, ...) |> 
    tidyr::pivot_wider(names_from = !!taxon,
                       values_from = !!Abundance,
                       values_fill = 0) |> 
    arrange(!!sample.name)
  
  samples <- pull(wide_tibble, !!sample.name)
  
  spp_matrix <- select(wide_tibble, all_of(taxa)) |>  data.matrix()
  rownames(spp_matrix) <- samples
  
  # Apply transformation if requested
  if (!is.null(transformation)) {
    if (transformation == "sq") {
      spp_matrix <- sqrt(spp_matrix)
    } else if (transformation %in% METHODS) {
      spp_matrix <- vegan::decostand(spp_matrix, method = transformation)
    }
  }
  
  return(vegan::vegdist(spp_matrix, method = distance))
}




#' Extract environmental metadata from a long tibble
#'
#' Returns the environmental data frame (sample metadata) from a long
#' sequence/OTU tibble by removing taxon and abundance columns.
#'
#' @param long.table A tibble with sample, taxon, abundance, and metadata columns.
#' @param taxon Column containing taxa/OTU IDs (unquoted).
#' @param Abundance Column with counts/abundance values (unquoted).
#' @param sample.name Column with sample IDs (unquoted).
#' @param ... Additional metadata columns to retain.
#'
#' @return A tibble of unique sample-level metadata.
#' @export
#' @rdname tibble_to_vegan
#' 
#' @examples
#' data("ASV_table")
#' data("metadata")
#' dplyr::inner_join(ASV_table,metadata) |> 
#' tibble_to_env(taxon = Hash, Abundance = nReads, sample.name = sample_name)
#' 

tibble_to_env <- function(long.table, taxon, Abundance, sample.name, ...) {
  taxon <- rlang::enquo(taxon)
  Abundance <- rlang::enquo(Abundance)
  sample.name <- rlang::enquo(sample.name)
  
  env <- long.table |> 
    select(-!!taxon, -!!Abundance) |> 
    distinct() |> 
    arrange(!!sample.name)
  
  return(env)
}
