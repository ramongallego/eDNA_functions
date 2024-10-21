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

read_info_file <- function (file, delim = "\t", col_names = TRUE, col_types = NULL, col_select = NULL, 
                            id = NULL, locale = default_locale(), na = c("", "NA"), quoted_na = TRUE, 
                            quote = "\"", comment = "", trim_ws = TRUE, skip = 0, n_max = Inf, 
                            guess_max = min(1000, n_max), name_repair = "unique", num_threads = readr_threads(), 
                            progress = show_progress(), show_col_types = should_show_types(), 
                            skip_empty_rows = TRUE, lazy = should_read_lazy()) 
{
  if (edition_first()) {
    tokenizer <- tokenizer_csv(na = na, quoted_na = quoted_na, 
                               quote = quote, comment = comment, trim_ws = trim_ws, 
                               skip_empty_rows = skip_empty_rows)
    return(read_delimited(file, tokenizer, col_names = col_names, 
                          col_types = col_types, locale = locale, skip = skip, 
                          skip_empty_rows = skip_empty_rows, comment = comment, 
                          n_max = n_max, guess_max = guess_max, progress = progress, 
                          show_col_types = show_col_types))
  }
  if (!missing(quoted_na)) {
    lifecycle::deprecate_soft("2.0.0", "readr::read_csv(quoted_na = )")
  }
  col_names <- c("Seq.id",
                    "n_errors",
                    "start_adap",
                    "end_adap",
                    "seq_before_adap",
                    "matching_seq",
                    "seq_after_adap",
                    "adap_name",
                    "QScores_seq_before",
                    "QScores_matching",
                    "QScores_after")
  
  vroom::vroom(file, delim = ",", col_names = col_names, col_types = col_types, 
               col_select = {
                 {
                   col_select
                 }
               }, id = id, .name_repair = name_repair, skip = skip, 
               n_max = n_max, na = na, quote = quote, comment = comment, 
               skip_empty_rows = skip_empty_rows, trim_ws = trim_ws, 
               escape_double = TRUE, escape_backslash = FALSE, locale = locale, 
               guess_max = guess_max, show_col_types = show_col_types, 
               progress = progress, altrep = lazy, num_threads = num_threads)
}