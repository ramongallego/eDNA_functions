test_that("tidy2phyloseq produces a phyloseq object", {
  # Load data objects
  data("ASV_table")
  data("metadata")
  data("OTU_taxonomy")
  data("tree")
  library(phyloseq)

  ps <- tidy2phyloseq(
    ASV_table = ASV_table,
    OTU_taxonomy = OTU_taxonomy,
    metadata = metadata,
    Taxa = "Hash",
    Sample = "sample_name",
    Reads = "nReads",
    tree = tree
  )
  
  # Check that it’s a phyloseq object
  expect_s4_class(ps, "phyloseq")
  
  # Check slots
  expect_true(!is.null(otu_table(ps)))
  expect_true(!is.null(sample_data(ps)))
  expect_true(!is.null(tax_table(ps)))
  if (!is.null(tree)) expect_true(!is.null(phy_tree(ps)))
})

test_that("phyloseq2tidy reproduces tidy data", {
  data("ASV_table")
  data("metadata")
  data("OTU_taxonomy")
  data("tree")
  
  ps <- tidy2phyloseq(
    ASV_table = ASV_table,
    OTU_taxonomy = OTU_taxonomy,
    metadata = metadata,
    Taxa = "Hash",
    Sample = "sample_name",
    Reads = "nReads",
    tree = tree
  )
  
  tidy_list <- phyloseq2tidy(
    phylo_obj = ps,
    Taxa = "Hash",
    Sample = "sample_name",
    Reads = "nReads"
  )
  
  expect_type(tidy_list, "list")
  expect_true(all(c("ASV_table", "taxonomy", "metadata") %in% names(tidy_list)))
  
  # Check ASV table consistency
  expect_true(all(unique(tidy_list$ASV_table$sample_name) %in% ASV_table$sample_name))
  expect_true(all(unique(tidy_list$ASV_table$Hash) %in% OTU_taxonomy$Hash))
  
  # Check taxonomy
  expect_true(all(tidy_list$taxonomy$Hash %in% OTU_taxonomy$Hash))
  
  # Check metadata
  expect_true(all(tidy_list$metadata$sample_name %in% metadata$sample_name))
})
