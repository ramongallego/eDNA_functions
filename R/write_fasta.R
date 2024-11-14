fasta_writer <- function(df, sequence, header, file.out){
  fasta <- character(nrow(df) * 2)
  fasta[c(TRUE, FALSE)] <- paste0(">", df %>% pull({{header}}))
  fasta[c(FALSE, TRUE)] <- df %>% pull({{sequence}})
  writeLines(fasta, file.out)
}

fastq_writer <- function(df, sequence, header,Qscores, file.out){
  fastq <- character(nrow(df) * 4)
  fastq[c(TRUE, FALSE, FALSE, FALSE)] <- paste0("@", df %>% pull({{header}}))
  fastq[c(FALSE, TRUE, F, F)] <- df %>% pull({{sequence}})
  fastq[c(F,F,T,F)] <- "+"
  fastq[c(F,F,F,T)] <- df %>% pull({{Qscores}})
  writeLines(fastq, file.out)
}
