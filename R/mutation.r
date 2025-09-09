#' Generate mutated DNA sequences
#'
#' This function takes DNA sequences and generates mutated variants.
#' Useful for testing classification algorithms on sequences with either PCR-induced or naturally occurring mutations.
#'
#' @param sequence A list of DNA sequences, either as "DNAbin" objects or character vectors.
#' @param format Character. Format of the input sequences. "bin" for DNAbin, "char" for character vectors.
#' @param n.mutations Integer. Number of mutations to introduce per sequence. Exclusive with prob.mutation.
#' @param prob.mutation Numeric. Probability of mutation per position. Exclusive with n.mutations.
#' 
#' @return A list of mutated sequences of the same class as the input.
#' @export
#' @importFrom stats runif
#'
#' @examples
#' data("test_seqs")
#' mutation(test_seqs, n.mutations = 2)
#' mutation(test_seqs, prob.mutation = 0.1)
#' seqs <- fasta_reader(system.file("extdata", "test.fasta", package="eDNAfuns"))
#' mutation(seqs$seq, format = "char", n.mutations = 1)
#' 
mutation <- function(sequence = NULL, format = "bin", n.mutations = NA, prob.mutation = NA) {
  
  if ((is.na(n.mutations) & is.na(prob.mutation)) | (!is.na(n.mutations) & !is.na(prob.mutation))) {
    stop("Please specify exactly one: n.mutations or prob.mutation")
  }
  # optionA we are given DNA string as a character
  if (format == "char") {
    options.for.mutations <- c("A", "C", "G", "T")
    
    mutated_sequences <- lapply(sequence, function(seq) {
      seq_length <- nchar(seq)
      
      # Determine positions to mutate
      if (!is.na(n.mutations)) {
        positions <- sample(seq_len(seq_length), n.mutations, replace = FALSE)
      } else {
        positions <- which(runif(seq_length) < prob.mutation)
      }
      
      # Split string into characters
      seq_vec <- strsplit(seq, "")[[1]]
      
      # Apply mutations
      for (pos in positions) {
        seq_vec[pos] <- sample(options.for.mutations[options.for.mutations != seq_vec[pos]], 1)
      }
      
      # Collapse back to string
      paste0(seq_vec, collapse = "")
    })
    
    class(mutated_sequences) <- "character"
  }else
    {
  
  # Extract unique options for mutations from the first sequence
  options.for.mutations <- unique(sequence[[1]])
  
  mutated_sequences <- lapply(sequence, function(seq) {
    seq_length <- length(seq)
    
    # Determine positions to mutate
    if (!is.na(n.mutations)) {
      if (n.mutations > seq_length) stop("n.mutations exceeds sequence length")
      positions <- sample(seq_len(seq_length), n.mutations, replace = FALSE)
    } else {
      positions <- which(runif(seq_length) < prob.mutation)
    }
    
    # Apply mutations
    seq[positions] <- sapply(seq[positions], function(nt) {
      sample(options.for.mutations[options.for.mutations != nt], 1)
    })
    
    return(seq)
  })
  class(mutated_sequences) <- "DNAbin"
    }
  
  
  return(mutated_sequences)
}
