fasta_writer <- function(df, sequence, header, file.out){
  fasta <- character(nrow(df) * 2)
  fasta[c(TRUE, FALSE)] <- paste0(">", df %>% pull({{header}}))
  fasta[c(FALSE, TRUE)] <- df %>% pull({{sequence}})
  writeLines(fasta, file.out)
}

