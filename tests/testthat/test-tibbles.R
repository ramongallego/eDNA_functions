# tests/testthat/test-tibble_to_vegan.R

library(testthat)
skip_if_not_installed("tidyverse")
library(tidyverse)
library(vegan)
library(eDNAfuns)  # replace with your actual package name

context("Making matrices and environmental datasets")

# Example input data
# Replace these with your actual training data if available
input <- left_join(training.ASV.table, training.metadata)

output_distance <- tibble_to_dist(
  long.table = input,
  taxon = Hash,
  Abundance = nReads,
  sample.name = Sample_name,
  distance = "bray",
  transformation = NULL
)

output_env <- tibble_to_env(
  long.table = input,
  taxon = Hash,
  Abundance = nReads,
  sample.name = Sample_name,
  everything()
)

test_that("Dimensions of output distance matrix match number of unique samples", {
  expect_equal(n_distinct(input$Sample_name), attr(output_distance, "Size"))
})

test_that("Order of samples in environmental data matches distance matrix labels", {
  expect_equal(output_env$Sample_name, attr(output_distance, "Labels"))
})

test_that("Number of rows in environmental data matches distance matrix size", {
  expect_equal(nrow(output_env), n_distinct(input$Sample_name))
})


