# A function that takes a long dataframe and turns it into a distance matrix

tibble_to_comm <- function (long.table,taxon,Abundance, sample.name){
  sample.name = rlang::enquo(sample.name)
  taxon = rlang::enquo(taxon) # quote taxon so it works with select
  Abundance = rlang::enquo(Abundance)
  
  long.table %>% 
    mutate(!!taxon := case_when(is.na(!!taxon) ~ "NA",
                                TRUE           ~ as.character(!!taxon))) -> long.table
  
  cols <- long.table %>% ungroup %>% dplyr::select(!!taxon) %>% distinct() %>% pull() # capture the unique values of the taxa
  
  
  
  long.table %>%
    ungroup %>% 
    select(!!taxon, !!Abundance, !!sample.name, ...) %>% # select at least three columns: nReads, taxa and the sampleID
    spread (key = !!taxon, value = !!Abundance, fill = 0) -> matrix_1
  
  samples <- pull (matrix_1, !!sample.name)
  dplyr::select (matrix_1, cols) -> matrix_1 # select only the spp
  data.matrix(matrix_1) -> matrix_1 # make it a data matrix
  
  dimnames(matrix_1)[[1]] <- samples
  
  
  return(matrix_1)
}



tibble_to_matrix <- function (long.table,taxon,Abundance, sample.name, distance = "bray", transformation, ...) {
  
  sample.name = rlang::enquo(sample.name)
  taxon = rlang::enquo(taxon) # quote taxon so it works with select
  choice = rlang::enquo(distance)
  Abundance = rlang::enquo(Abundance)
  
  
  # So now the eDNA index works let's see if we can use decostand
  
  # In case there are NAs in teh taxa, the thing breaks - 
  
  ## 
  
  long.table %>% 
    mutate(!!taxon := case_when(is.na(!!taxon) ~ "NA",
                                TRUE           ~ as.character(!!taxon))) -> long.table
  
  cols <- long.table %>% ungroup %>% dplyr::select(!!taxon) %>% distinct() %>% pull() # capture the unique values of the taxa
  
  
   
  long.table %>%
    ungroup %>% 
    select(!!taxon, !!Abundance, !!sample.name, ...) %>% # select at least three columns: nReads, taxa and the sampleID
    spread (key = !!taxon, value = !!Abundance, fill = 0) -> wide_tibble # Make a wide table, like vegan likes
   wide_tibble %>% arrange(!!sample.name) -> wide.rearranged
  #  return(wide_tibble$Sample_name) 
  # return (head(wide.rearranged))
  sample.col <- colnames(wide_tibble)[!colnames(wide_tibble) %in% cols] # Which column has all the sample info
  #return(env)
  dplyr::select (wide.rearranged, sample.col) -> samples # The values of the sample info in the order they appear in the wide table
  # return(samples)
  samples <- pull (wide.rearranged, !!sample.name)
 # return(samples)
  dplyr::select (wide.rearranged, cols) -> wide_spp # select only the spp
   #return(wide_spp)
  data.matrix(wide_spp) -> matrix_1 # make it a data matrix
   #return(str(matrix_1))
  dimnames(matrix_1)[[1]] <- samples # name it with the sample info
   #return(str(matrix_1))
  # Now that the matrix is ready to go, we can apply decostand if needed
  
  good.transformations <- c("total", "max", "frequency", "normalize", "range", 
                            "rank", "rrank", "standardize", "pa", "chi.square", "hellinger", 
                            "log")
  if (transformation == "sq") {
    sqrt(matrix_1) -> matrix_1
  }else{
  if(transformation %in%  good.transformations ){
    decostand(matrix_1, method = transformation) -> matrix_1
  }
  }
  #simper(matrix_1, env$Site)
  vegdist(matrix_1, method = distance) -> matrix_1 
  return(matrix_1)
  # calculate the dissimilarity matrix
  
#   adonis(matrix_1 ~  Site*Date, data= env)
  # as.tibble(subset(melt(as.matrix(matrix_1))))
}

tibble_to_permanova <- function (long.table,taxon,Abundance, sample.name, distance = "bray", transformation, formula = "",...) {
  
  sample.name = rlang::enquo(sample.name)
  taxon = rlang::enquo(taxon) # quote taxon so it works with select
  choice = rlang::enquo(distance)
  Abundance = rlang::enquo(Abundance)
  
  
  # So now the eDNA index works let's see if we can use decostand
  
  
  
  cols = long.table %>% ungroup %>% dplyr::select(!!taxon) %>% distinct() %>% pull() # capture the unique values of the taxa
  
  
  long.table %>%
    ungroup %>% 
    select(!!taxon, !!Abundance, !!sample.name, ...) %>% # select at least three columns: nReads, taxa and the sampleID
    spread (key = !!taxon, value = !!Abundance, fill = 0) -> matrix_1 # Make a wide table, like vegan likes
  
  env <- colnames(matrix_1)[!colnames(matrix_1) %in% cols] # Which column has all the sample info
  
  dplyr::select (matrix_1, env) -> env # The values of the sample info in the order they appear in the wide table
  
  samples <- pull (matrix_1, !!sample.name)
  dplyr::select (matrix_1, cols) -> matrix_1 # select only the spp
  data.matrix(matrix_1) -> matrix_1 # make it a data matrix
  
  dimnames(matrix_1)[[1]] <- samples # name it with the sample info
  
  # Now that the matrix is ready to go, we can apply decostand if needed
  
  good.transformations <- c("total", "max", "frequency", "normalize", "range", 
                            "rank", "rrank", "standardize", "pa", "chi.square", "hellinger", 
                            "log")
  if (transformation == "sq") {
    sqrt(matrix_1) -> matrix_1
  }else{
    if(transformation %in%  good.transformations ){
      decostand(matrix_1, method = transformation) -> matrix_1
    }
  }
  #simper(matrix_1, env$Site)
  vegdist(matrix_1, method = distance) -> matrix_1 
  return(matrix_1)
  # calculate the dissimilarity matrix
  
     adonis(matrix_1 ~  Site*Date, data= env)
  # as.tibble(subset(melt(as.matrix(matrix_1))))
}


tibble_to_env <- function (long.table,taxon,Abundance,sample.name, ...) {  # A function that returns the accompaining environmental matrix  
  
  # Enquos to allow me to play with the data
  
  sample.name = rlang::enquo(sample.name)
  taxon = rlang::enquo(taxon) # quote taxon so it works with select
  #choice = rlang::enquo(distance)
  Abundance = rlang::enquo(Abundance)
  
  cols <- long.table %>% ungroup %>% dplyr::select(!!taxon) %>% distinct() %>% pull() # capture the unique values of the taxa
   #return(cols)
  long.table %>%
    ungroup %>% 
    # select(!!taxon, !!Abundance, !!sample.name, ...) %>% 
    select( -!!taxon, -!!Abundance) %>% 
    distinct()-> wide_tibble
    # spread (key = !!taxon, value = !!Abundance, fill = 0) -> wide_tibble
  wide_tibble %>% arrange(!!sample.name) -> wide_tibble
   # return (head(wide.rearranged))
  env <- colnames(wide_tibble)[!colnames(wide_tibble) %in% cols]
  # return(env)
  dplyr::select (wide_tibble, env) -> env
  env %>% distinct()-> env
  return(env)
 
  
}

