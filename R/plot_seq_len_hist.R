#' Plot sequence length distribution
#'
#' This function takes a tibble produced by `fasta_reader()` or `fastq_reader()`
#' and plots the distribution of sequence lengths.
#'
#' @param Hash_tibble A tibble containing a numeric column with sequence lengths.
#' @param length_col The column of `Hash_tibble` containing sequence lengths (unquoted).
#' @param binwidth Width of histogram bins. Default = 1.
#' @param label_interval Interval for x-axis labels. Default = 5.
#'
#' @importFrom rlang enquo
#' @importFrom dplyr pull
#' @importFrom ggplot2 ggplot aes geom_histogram scale_x_continuous theme_minimal labs
#' 
#' @return A ggplot object.
#' @export
#'
#' @examples
#' data("example_hashes")
#' plot_seq_len_hist(example_hashes, seq_len)
plot_seq_len_hist <- function(Hash_tibble, length_col, binwidth = 1, label_interval = 5) {
  length_col <- rlang::enquo(length_col)
  
  x_min <- min(dplyr::pull(Hash_tibble, !!length_col), na.rm = TRUE)
  x_max <- max(dplyr::pull(Hash_tibble, !!length_col), na.rm = TRUE)
  x_breaks <- seq(x_min, x_max, by = 1)
  x_labels <- ifelse(x_breaks %% label_interval == 0, x_breaks, "")
  
  p <- Hash_tibble |> 
    ggplot2::ggplot(ggplot2::aes(x = !!length_col)) +
    ggplot2::geom_histogram(binwidth = binwidth, color = "black", fill = "steelblue") +
    ggplot2::scale_x_continuous(breaks = x_breaks, labels = x_labels) +
    ggplot2::theme_minimal() +
    ggplot2::labs(y = "Count (unique Hashes)", x = "Sequence length")
  
  return(p)
}
