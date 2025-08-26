context("eDNAindex works directly ")
data("training.ASV.table")
library(tidyverse)
output <- eDNAindex(training.ASV.table, Sample_name, Hash, nReads )

Hash1<-  output %>% group_by(Hash) %>% tally(sort = T) %>% slice(1) %>% pull(Hash)

test_that("length of output is similar to input ", {
  expect_equal(nrow(training.ASV.table), nrow(output))
  
})

test_that(" values of each ASV are different", {
  expect_true((output %>% 
                filter (Hash == Hash1) %>%
                summarise(n_distinct(Normalized.reads)) %>%
                pull)>1)
  })

test_that("All ASVs get a max of 1", {
  expect_equal(output %>%
                 group_by (Hash) %>%
                 summarise(max = max(Normalized.reads)) %>%
                  ungroup() %>% 
                  distinct(max) %>% 
                 pull, 1)
                 
})

## With pooling


input <- training.ASV.table %>%
  separate(Sample_name, into = c("Biol", "PCR.replicate"), remove=F, sep = -1)
output <- eDNAindex(tibble = input,
                    Sample_column = Sample_name,OTU_column = Hash,Counts_column =  nReads,Biological_replicate_column = Biol )

test_that("length of output is similar to input ", {
  expect_equal(nrow(training.ASV.table), nrow(output))
  
})
test_that(" values of each ASV are different", {
  expect_gt((output %>% 
                 filter (Hash == Hash1) %>%
                 summarise(n_distinct(Normalized.reads)) %>%
                 pull),1)
})

test_that("All ASVs get a max of 1", {
  expect_equal(output %>%
                 group_by (Hash) %>%
                 summarise(max = max(Normalized.reads)) %>%
                 ungroup() %>% 
                 distinct(max) %>% 
                 pull, 1)
  
})


## carryover works
data("training.metadata")

input <- training.ASV.table |> 
  inner_join(training.metadata |> 
               select(Sample_name, Transect, depth, eDNA.sample)) 
output <- eDNAindex(input,
                    Sample_column = eDNA.sample,
                    OTU_column = Hash,
                    Counts_column = nReads,
                    Biological_replicate_column = Sample_name,
                    Transect, depth, Locus)
test_that("colnames are kept",{
  
  expect_setequal(input %>% select(-nReads, -Sample_name) %>% names(.) ,
               output %>% select(-Normalized.reads) %>% names(.))
  
  
})
