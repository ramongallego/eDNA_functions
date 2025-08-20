library(testthat)
library(ape)  # for DNAbin example data
library(insect)  # if needed for your data

context("mutation function works as expected")

wood_chars <- lapply(woodmouse, dna2char)[[1]]

# Test: output has same length as input
test_that("output length matches input length", {
  mutated <- mutation(woodmouse, format = "bin", n.mutations = 3)
  expect_equal(length(mutated), length(woodmouse))
})
test_that("output length matches input length in chars", {
  mutated <- mutation(wood_chars, format = "char", n.mutations = 3)
  expect_equal(nchar(mutated), nchar(wood_chars))
})


# Test: mutated positions differ from original
test_that("mutated sequences differ from original", {
  mutated <- mutation(woodmouse, format = "bin", n.mutations = 3)
  differences <- mapply(function(orig, mut) any(orig != mut), woodmouse, mutated)
  expect_true(any(differences))
})
test_that("mutated sequences differ from original in chars", {
  mutated <- mutation(wood_chars, format = "char", n.mutations = 3)
  
  expect_false(mutated==wood_chars)
})


# Test: error when both or neither mutation arguments are provided
test_that("error triggers with invalid arguments", {
  expect_output(mutation(woodmouse, format = "bin", n.mutations = NA, prob.mutation = NA),
                "Error")
  expect_output(mutation(woodmouse, format = "bin", n.mutations = 2, prob.mutation = 0.1),
                "Error")
})



# Test: class of output is DNAbin
test_that("output is DNAbin class", {
  mutated <- mutation(woodmouse, format = "bin", n.mutations = 3)
  expect_s3_class(mutated, "DNAbin")
})

# Test: class of output is Char
test_that("output is character class", {
  mutated <- mutation(wood_chars[[1]], format = "char", n.mutations = 3)
  expect_s3_class(mutated, "char")
})
