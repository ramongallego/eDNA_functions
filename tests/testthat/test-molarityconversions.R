test_that("ng2nM and nM2ng are inverse", {
  length_amp <- 300
  mass <- 10  # ng/uL
  conc <- ng2nM(mass, length_amp)
  expect_equal(nM2ng(conc, length_amp), mass)
})

test_that("output is numeric", {
  expect_type(ng2nM(5, 150), "double")
  expect_type(nM2ng(10, 150), "double")
})
