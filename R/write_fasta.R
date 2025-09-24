#' Read FASTA or FASTQ files into a tibble
#'
#' Functions to read sequence files into a tidy data frame with one row per sequence.
#' @param df A dataframe where the sequence information is stored, one sequence per row
#' @param header The name (unquoted) of the column where the header information is stored
#' @param sequence The name (unquoted) of the column where the sequence information (as characters) is stored
#' @param Qscores The name (unquoted) of the column where the Quality information (encoded as characters) is stored
#' @param file.out Character. Path to the location where to write the file.
#' 
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
#' fasta_file <- tempfile(fileext = ".fasta")
#' 
#' fasta_df <- fasta_reader(system.file("extdata", "test.fasta", package="eDNAfuns"))
#' 
#' fasta_writer(fasta_df, sequence=seq,
#'               header = header,
#'               file.out = fasta_file)
#'  
#' fastq_file <- tempfile(fileext = ".fastq")  
#'            
#' fastq_df <- fastq_reader(system.file("extdata", "test.fastq", package="eDNAfuns"), keepQ = TRUE)
#' 
#' fastq_writer(fastq_df, sequence=seq,
#'               header = header,Qscores= Qscores,
#'               file.out = fastq_file)
#'
#' @export
fasta_writer <- function(df, sequence, header, file.out){
  fasta <- character(nrow(df) * 2)
  fasta[c(TRUE, FALSE)] <- paste0(">", df |> pull({{header}}))
  fasta[c(FALSE, TRUE)] <- df|> pull({{sequence}})
  writeLines(fasta, file.out)
}

#' @rdname fasta_writer
#' @export


fastq_writer <- function(df, sequence, header,Qscores, file.out){
  fastq <- character(nrow(df) * 4)
  fastq[c(TRUE, FALSE, FALSE, FALSE)] <- paste0("@", df|> pull({{header}}))
  fastq[c(FALSE, TRUE, F, F)] <- df |> pull({{sequence}})
  fastq[c(F,F,T,F)] <- "+"
  fastq[c(F,F,F,T)] <- df |> pull({{Qscores}})
  writeLines(fastq, file.out)
}
