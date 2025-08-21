test_that("fasta_reader reads fasta into tibble", {
  fasta_file <- system.file("extdata", "test.fasta", package = "eDNAfuns")
  expect_true(file.exists(fasta_file))
  
  fasta <- fasta_reader(fasta_file)
  
  expect_s3_class(fasta, "tbl_df")
  expect_equal(colnames(fasta), c("header", "seq"))
  expect_gt(nrow(fasta), 0)
  expect_type(fasta$header, "character")
  expect_type(fasta$seq, "character")
})

test_that("fastq_reader reads fastq without Qscores", {
  fastq_file <- system.file("extdata", "test.fastq", package = "eDNAfuns")
  expect_true(file.exists(fastq_file))
  
  fastq <- fastq_reader(fastq_file, keepQ = FALSE)
  
  expect_s3_class(fastq, "tbl_df")
  expect_equal(colnames(fastq), c("header", "seq"))
  expect_gt(nrow(fastq), 0)
})

test_that("fastq_reader reads fastq with Qscores", {
  fastq_file <- system.file("extdata", "test.fastq", package = "eDNAfuns")
  fastq <- fastq_reader(fastq_file, keepQ = TRUE)
  
  expect_s3_class(fastq, "tbl_df")
  expect_equal(colnames(fastq), c("header", "seq", "Qscores"))
  expect_gt(nrow(fastq), 0)
  expect_type(fastq$Qscores, "character")
})
