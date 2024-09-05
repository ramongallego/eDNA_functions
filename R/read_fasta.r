# I like it when I can have fasta sequences loaded into R as dataframes (tibbles),
# with one column for the header and one column for the DNA sequence
# Maybe the read_fastq should have the posibility of keeping a third column with the Qscores?

fasta_reader <- function(path_to_fasta){
  
  temp <- readLines(path_to_fasta)
  
  tibble (header = substr(temp[c(T,F)],2,500),
          seq = temp[c(F,T)]) 
  
}

fastq_reader <- function(path_to_fastq, keepQ=F){
  
  temp <- readLines(path_to_fastq)
  
  if(keepQ==F){
  
    tibble (header = substr(temp[c(T,F,F,F)],2,500),
            seq = temp[c(F,T, F, F)]) 
    
  }else{
    
    tibble(header = substr(temp[c(T,F,F,F)],2,500),
           seq = temp[c(F,T, F, F)],
           Qscores=temp[c(F,F,F,T)])
  }
  
  
  
}