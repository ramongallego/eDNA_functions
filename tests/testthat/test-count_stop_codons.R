test_that("count_stop_codons works with DNAString input", {
  skip_if_not_installed("Biostrings")
  library(Biostrings)
  
  seq <- DNAString("ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG") # standard example
  
  # Default: count stop codons
  expect_type(count_stop_codons(seq), "integer")
  expect_equal(count_stop_codons(seq, codon = 1, dictionary = 1), 2)
  
  # Translation instead of count
  aa_seq <- count_stop_codons(seq, codon = 1, dictionary = 1, return = "translation")
  expect_type(aa_seq, "character")
  expect_true(grepl("\\*", aa_seq))
})

test_that("count_stop_codons works with character input", {
  seq <- "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG"
  
  expect_equal(count_stop_codons(seq, format = "character", codon = 1, dictionary = 1), 2)
  
  # Translation string
  aa_seq <- count_stop_codons(seq, format = "character", codon = 1, dictionary = 1, return = "translation")
  expect_type(aa_seq, "character")
  expect_true(grepl("\\*", aa_seq))
})

test_that("count_stop_codons respects codon offset", {
  seq <- "ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG"
  
  # Starting at codon 2 or 3 should yield different results
  count1 <- count_stop_codons(seq, format = "character", codon = 1)
  count2 <- count_stop_codons(seq, format = "character", codon = 2)
  count3 <- count_stop_codons(seq, format = "character", codon = 3)
  
  expect_false(all(c(count1, count2, count3) == count1))
})

test_that("count_stop_codons warns on non-ACGT characters", {
  bad_seq <- "ATGNCC"  # contains "N"
  
  expect_error(
    count_stop_codons(bad_seq, format = "character")
  )
})
