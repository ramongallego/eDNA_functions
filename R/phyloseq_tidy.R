#' Convert a tidy ASV table to a phyloseq object
#'
#' This function converts a tidy ASV table, along with optional taxonomy and metadata,
#' into a `phyloseq` object. 
#'
#' @param ASV_table A tidy data.frame/tibble of ASV counts.
#' @param OTU_taxonomy A data.frame with OTU taxonomy. Optional.
#' @param metadata A data.frame with sample metadata. Optional.
#' @param Taxa Column name in OTU_taxonomy/ASV_table corresponding to OTU IDs (default: "sseqid").
#' @param Sample Column name for sample IDs (default: "sample_name").
#' @param Reads Column name for read counts (default: "nr").
#' @param tree A phylogenetic tree of class `phylo`. Optional.
#'
#' @return A `phyloseq` object combining OTU table, taxonomy, metadata, and optionally a tree.
#'
#' @rdname phyloseq_tidy
#' @examples
#' \dontrun{
#' ps <- tidy2phyloseq(ASV_table = my_ASV, OTU_taxonomy = my_tax, metadata = my_meta)
#' }
#'
#' @import phyloseq dplyr tidyr tibble
#' @export
tidy2phyloseq <- function(ASV_table,
                          OTU_taxonomy = NULL,
                          metadata = NULL,
                          Taxa = "sseqid", 
                          Sample = "sample_name",
                          Reads = "nr", 
                          tree = NULL) {
  
  # Taxonomy table
  if(!is.null(OTU_taxonomy)) {
    newtax <- OTU_taxonomy %>%
      semi_join(ASV_table, by = Taxa) %>%
      column_to_rownames(Taxa) %>%
      as.matrix() %>%
      tax_table()
  }
  
  # Metadata
  if(!is.null(metadata)) {
    newsample <- metadata %>%
      semi_join(ASV_table, by = Sample) %>%
      column_to_rownames(Sample) %>%
      sample_data()
  } else newsample <- NULL
  
  # ASV table
  newotu <- ASV_table %>%
    pivot_wider(names_from = {{Taxa}}, values_from = {{Reads}}, values_fill = 0) %>%
    column_to_rownames(Sample) %>%
    otu_table(taxa_are_rows = FALSE)
  
  # Construct phyloseq object
  if(!is.null(OTU_taxonomy)) {
    phyloseq(newotu, newsample, newtax, tree)
  } else {
    phyloseq(newotu, newsample, tree)
  }
}

#' Convert a phyloseq object to a tidy ASV table
#'
#' Extracts ASV counts, taxonomy, and metadata from a `phyloseq` object into tidy data frames.
#'
#' @param phylo_obj A phyloseq object.
#' @param Taxa Column name for OTU IDs in the output (default: "sseqid").
#' @param Sample Column name for sample IDs in the output (default: "sample_name").
#' @param Reads Column name for read counts in the output (default: "nr").
#'
#' @return A list with three tibbles:
#' \describe{
#'   \item{ASV_table}{Long-format tibble of counts: Sample, Taxa, Reads.}
#'   \item{taxonomy}{Taxonomy table as tibble (NULL if none in phyloseq).}
#'   \item{metadata}{Sample metadata as tibble (NULL if none in phyloseq).}
#' }
#' 
#' @rdname phyloseq_tidy
#'
#' @examples
#' \dontrun{
#' tidy_list <- phyloseq2tidy(my_phyloseq)
#' }
#'
#' @import phyloseq dplyr tidyr tibble
#' @export
phyloseq2tidy <- function(phylo_obj,
                          Taxa = "sseqid",
                          Sample = "sample_name",
                          Reads = "nr") {
  
  # helper to convert data.frame to tibble with rownames column
  df_to_tibble <- function(df, rowname_col) {
    df %>%
      as.data.frame() %>%
      rownames_to_column(var = rowname_col) %>%
      as_tibble()
  }
  
  ## ASV table (long format)
  otu_long <- otu_table(phylo_obj) %>%
    as.data.frame() %>%
    df_to_tibble(rowname_col = Sample) %>%
    pivot_longer(
      cols = -all_of(Sample),
      names_to = Taxa,
      values_to = Reads
    ) %>%
    filter(.data[[Reads]] > 0)
  
  ## Taxonomy table
  if (!is.null(tax_table(phylo_obj, errorIfNULL = FALSE))) {
    tax_tab <- tax_table(phylo_obj) %>%
      as.matrix() %>%
      df_to_tibble(rowname_col = Taxa)
  } else {
    tax_tab <- NULL
  }
  
  ## Sample metadata
  if (!is.null(sample_data(phylo_obj, errorIfNULL = FALSE))) {
    meta_tab <- sample_data(phylo_obj) %>%
      as.data.frame() %>%
      df_to_tibble(rowname_col = Sample)
  } else {
    meta_tab <- NULL
  }
  
  return(list(
    ASV_table = otu_long,
    taxonomy = tax_tab,
    metadata = meta_tab
  ))
}