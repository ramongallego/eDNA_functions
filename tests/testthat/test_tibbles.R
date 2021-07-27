context("Making matrices and environmental datasets")


left_join(ASV.table, metadata) -> input
output_distance <- tibble_to_matrix(long.table = input,taxon =Hash, Abundance = nReads, sample.name = Sample_name,
                                    distance = "bray",transformation = "")

output_env <- tibble_to_env(long.table = input,taxon = Hash, Abundance = nReads, sample.name = Sample_name, everything())

test_that("Dimensions of output are similar to n_distinct input ", {
  expect_equal(n_distinct(input$Sample_name), attr(output_distance, "Size"))
})

test_that("order of samples is the same", {
  expect_equal(output_env$Sample_name, attr(output_distance, "Labels"))
  })

test_that("nRows of environmental data are the same as the dissimilarity matrix", {
  expect_equal  (nrow(output_env), n_distinct(input$Sample_name))
                 
})

