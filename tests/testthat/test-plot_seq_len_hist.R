test_that("plot_seq_len_hist works with example data", {
  data("example_hashes")
  
  p <- plot_seq_len_hist(example_hashes, seq_len)
  
  expect_s3_class(p, "ggplot")
  expect_true(any(grepl("Count", p$labels$y)))
  expect_true(any(grepl("Sequence length", p$labels$x)))
})
