tidy2phyloseq <- function (ASV_table = ASV_collapsed_by_BLAST,
                           OTU_taxonomy = OTU_tax,
                           metadata = metadata_rarefied,
                           Taxa = "sseqid", 
                           Sample= "sample_name",
                           Reads = "nr", 
                           tree=NULL){
  # reduce dfs to equal dimmensions
  
  ## OTU_table
  if(!is.null(OTU_taxonomy)){
  OTU_taxonomy |> 
    semi_join(ASV_table) |> 
    as.data.frame() -> midstep
  midstep |> 
    column_to_rownames(Taxa) |> 
    tax_table() -> newtax
  midstep |> 
    pull(Taxa)   -> dimnames(newtax)[[1]]
  
  midstep |> 
    column_to_rownames(Taxa) |> 
    colnames()-> dimnames(newtax)[[2]]}
  
  ## Metadata
  metadata |> 
    semi_join(ASV_table) |> 
    as.data.frame() |> 
    column_to_rownames(Sample) |> 
    sample_data() -> newsample
  
  ## ASV
  ASV_table |>
    pivot_wider(names_from = {{Taxa}}, values_from = {{Reads}}, values_fill = 0) |> 
    as.data.frame() |> 
    column_to_rownames(Sample) |> 
    otu_table(taxa_are_rows = F) -> newotu
  
  

  phyloseq(newsample,newotu, newtax,tree)
  
  
}

phyloseq2tidy <- function (phylo_obj = phylo_obj,
                           ASV_table = ASV_collapsed_by_BLAST,
                           OTU_taxonomy = OTU_tax,
                           metadata = metadata_rarefied,
                           Taxa = "sseqid", 
                           Sample= "sample_name",
                           Reads = "nr", 
                           tree=NULL){
  # reduce dfs to equal dimmensions
  
  ## OTU_table
  
  otu_table(phylo_obj) |>
    as.data.frame() |>
    rownames_to_column(Sample) |>
    pivot_longer(-{{Sample}}, names_to = Taxa, values_to = Reads) |>
    filter (nr>0) -> temp
  
  assign(ASV_table, temp)
  
  
  ## Metadata
  metadata |> 
    semi_join(ASV_table) |> 
    as.data.frame() |> 
    column_to_rownames(Sample) |> 
    sample_data() -> newsample
  
  ## ASV
  ASV_table |>
    pivot_wider(names_from = {{Taxa}}, values_from = {{Reads}}, values_fill = 0) |> 
    as.data.frame() |> 
    column_to_rownames(Sample) |> 
    otu_table(taxa_are_rows = F) -> newotu
  
  
  
  phyloseq(newtax,newsample,newotu, tree)
  
  
}
