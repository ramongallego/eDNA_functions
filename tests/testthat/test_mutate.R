context("generate mutated sequences ")



output <- mutation(test_seqs, n.mutations = 5)



test_that("length of output is similar to input ", {
  expect_equal(length(test_seqs), length(output))
})

test_that("length of each sequence output is similar to input", {
  expect_equal(map(test_seqs, length), map(output, length))
  })

test_that("each sequence is different from the original one", {
  expect_equal  (map2(test_seqs, output, ~ summary (.x == .y)["FALSE"] %>% 
                                          
                                         as.double()) %>% 
                    keep(. > 0) %>% 
                      length, length(output))
                 
})

