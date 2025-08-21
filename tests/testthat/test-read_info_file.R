# tests/testthat/test-read_info_file.R

test_that("Illumina cutadapt info file is read correctly", {
  path <- system.file("extdata", "cutadapt_info_illumina.txt", package = "eDNAfuns")
  df <- read_info_file(path)
  
  # Basic structure
  expect_s3_class(df, "tbl_df")
  expect_true(all(c("Seq.id", "n_errors", "start_adap", "end_adap") %in% names(df)))
  
  # Illumina file has expected number of columns
  expect_equal(ncol(df), 11)
  
  # n_errors should be numeric/integer
  expect_type(df$n_errors, "integer")
})


test_that("Nanopore cutadapt info file is read correctly", {
  path <- system.file("extdata", "cutadapt_info_nanopore.txt", package = "eDNAfuns")
  df <- read_info_file_nanopore(path)
  
  # Basic structure
  expect_s3_class(df, "tbl_df")
  expect_true(all(c("Seq.id", "n_errors", "start_adap", "end_adap", "matching_seq") %in% names(df)))
  
  # Nanopore file has expected number of columns
  expect_equal(ncol(df), 14)
  
  # QScores columns exist and are character
  expect_type(df$QScores_matching, "character")
})
