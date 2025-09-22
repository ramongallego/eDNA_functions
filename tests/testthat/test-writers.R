test_that("fasta_writer writes and reads correctly", {
  # make a minimal tibble
  df <- tibble::tibble(
    header = c("seq1", "seq2"),
    seq = c("ATGC", "GGCCAATT")
  )
  
  # create a temp file (deleted after session)
  fasta_file <- tempfile(fileext = ".fasta")
  
  # write to fasta
  fasta_writer(df, sequence = seq, header = header, file.out = fasta_file)
  
  # read it back with your own fasta_reader
  read_df <- fasta_reader(fasta_file)
  
  # check dimensions and content
  expect_equal(nrow(read_df), 2)
  expect_equal(read_df$header, df$header)
  expect_equal(read_df$seq, df$seq)
})

test_that("fastq_writer writes and reads correctly", {
  # make a minimal tibble
  df <- tibble::tibble(
    header = c("seq1", "seq2"),
    seq = c("ATGC", "GGCCAATT"),
    Qscores = c("IIII", "JJJJJJJJ")
  )
  
  # create a temp file
  fastq_file <- tempfile(fileext = ".fastq")
  
  # write to fastq
  fastq_writer(df, sequence = seq, header = header,
               Qscores = Qscores, file.out = fastq_file)
  
  # read it back with your fastq_reader (with keepQ = TRUE)
  read_df <- fastq_reader(fastq_file, keepQ = TRUE)
  
  # check dimensions and content
  expect_equal(nrow(read_df), 2)
  expect_equal(read_df$header, df$header)
  expect_equal(read_df$seq, df$seq)
  expect_equal(read_df$Qscores, df$Qscores)
})
