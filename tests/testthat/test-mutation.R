library(testthat)
skip_if_not_installed("insect")
library(insect)  # if needed for your data

context("mutation function works as expected")

char2dna(list(seq1 = "TAACGC", seq2 = "ATTGCG")) -> test_seq
dna2char(test_seq) -> wood_chars
mutated_bin  <- mutation(test_seq, format = "bin", n.mutations = 1)
mutated_char <- mutation(wood_chars, format = "char", n.mutations = 1)
# Test: output has same length as input
test_that("output length matches input length with many seqs in input", {
  
  dna2char(test_seq) |> enframe() -> comparable_woodmouse
  dna2char(mutated_bin) |> enframe() -> comparable_output
  
  expect_equal(nchar(comparable_woodmouse$value), nchar(comparable_output$value))
})

test_that("output length matches input length in chars with many seqs in input", {
  
  expect_equal(nchar(mutated_char), nchar(wood_chars))
})


# Test: mutated positions differ from original
test_that("mutated sequences differ from original", {
 
  differences <- mapply(function(orig, mut) any(orig != mut), test_seq, mutated_bin)
  expect_true(all(differences))
})
test_that("mutated sequences differ from original in chars", {
  differences <- mapply(function(orig, mut) any(orig != mut), wood_chars, mutated_char)
  
  expect_true(all(differences))
})


# Test: error when both or neither mutation arguments are provided
test_that("error triggers with invalid arguments", {
  expect_error(mutation(test_seq, format = "bin", n.mutations = NA, prob.mutation = NA))
  expect_error(mutation(test_seq, format = "bin", n.mutations = 2, prob.mutation = 0.1))
  expect_error(mutation(wood_chars, format = "char", n.mutations = NA, prob.mutation = NA))
  expect_error(mutation(wood_chars, format = "char", n.mutations = 2, prob.mutation = 0.1))
})



# Test: class of output is DNAbin
test_that("output is DNAbin class", {
 
  expect_s3_class(mutated_bin, "DNAbin")
})

# Test: class of output is Char
test_that("output is character class", {

  expect_type(mutated_char, "character")
})
# 