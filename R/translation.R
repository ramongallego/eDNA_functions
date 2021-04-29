# A function that takes a DNA sequence (in character or DNAbin),
# A starting codon
# A translation dictionary
# And returns the number of stop codons found.

count_stop_codons <- function(sequence = NULL, format = "DNAString", codon = 1, dictionary = 5, return = "count"){
  
  require(tidyverse)
  require(Biostrings)
  
 # Get the genetic Code
  code <- getGeneticCode(id_or_name2 = as.character(dictionary))
  
  if (format != "DNAString"){
    if(str_detect(sequence, "[^ACGT]")){
      return(print("WARNING: Non ACGT characters found "))
    }
    input <- DNAString(sequence)} else { input <- sequence}
  
  translation <-  as.character(suppressWarnings(translate(subseq(input,start = codon),
                                                          genetic.code = code,
                                                          no.init.codon = T)))
  if(return != "count"){
    return(translation) } else {
  return(str_count(translation, "\\*"))}
                                                
}
