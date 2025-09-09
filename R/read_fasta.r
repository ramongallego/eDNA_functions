#' Read FASTA or FASTQ files into a tibble
#'
#' Functions to read sequence files into a tidy data frame with one row per sequence.
#'
#' @param path_to_fasta Character. Path to a FASTA file.
#' @param path_to_fastq Character. Path to a FASTQ file.
#' @param keepQ Logical. If `TRUE`, keep a third column with quality scores when
#'   reading FASTQ files. Default is `FALSE`.
#'
#' @return
#' - `fasta_reader()`: A tibble with columns:
#'   - `header`: sequence identifiers (without the `>`).
#'   - `seq`: nucleotide sequences.
#'
#' - `fastq_reader()`: A tibble with columns:
#'   - `header`: sequence identifiers (without the `@`).
#'   - `seq`: nucleotide sequences.
#'   - `Qscores` (optional): quality scores, if `keepQ = TRUE`.
#'
#' @examples
#' 
#' fasta_df <- fasta_reader(system.file("extdata", "test.fasta", package="eDNAfuns"))
#' fastq_df <- fastq_reader(system.file("extdata", "test.fastq", package="eDNAfuns"), keepQ = TRUE)
#' 
#'
#' @export
fasta_reader <- function(path_to_fasta) {
  temp <- readLines(path_to_fasta)
  tibble::tibble(
    header = substr(temp[c(TRUE, FALSE)], 2, 500),
    seq    = temp[c(FALSE, TRUE)]
  )
}

#' @rdname fasta_reader
#' @export
fastq_reader <- function(path_to_fastq, keepQ = FALSE) {
  temp <- readLines(path_to_fastq)
  if (!keepQ) {
    tibble::tibble(
      header = substr(temp[c(TRUE, FALSE, FALSE, FALSE)], 2, 500),
      seq    = temp[c(FALSE, TRUE, FALSE, FALSE)]
    )
  } else {
    tibble::tibble(
      header  = substr(temp[c(TRUE, FALSE, FALSE, FALSE)], 2, 500),
      seq     = temp[c(FALSE, TRUE, FALSE, FALSE)],
      Qscores = temp[c(FALSE, FALSE, FALSE, TRUE)]
    )
  }
}
