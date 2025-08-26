#' Count stop codons or return translated sequence
#'
#' Given a DNA sequence (either a `DNAString` object or a character string),
#' this function translates it using a specified genetic code and counts the
#' number of stop codons (`*`) in the resulting amino acid sequence.
#' Alternatively, the translated sequence itself can be returned.
#'
#' @param sequence A DNA sequence, either as a [Biostrings::DNAString] object
#'   or as a character string.
#' @param format Input format, either `"DNAString"` (default) or `"character"`.
#'   If `"character"`, the sequence must consist of `A`, `C`, `G`, `T` only.
#' @param codon Integer giving the starting codon position (usually 1, 2, or 3).
#' @param dictionary Integer specifying which translation code to use.
#'   See [Biostrings::GENETIC_CODE_TABLE] for all options. Common examples:
#'
#'   \tabular{ll}{
#'     \strong{id} \tab \strong{name} \cr
#'     1  \tab Standard \cr
#'     2  \tab Vertebrate Mitochondrial \cr
#'     3  \tab Yeast Mitochondrial \cr
#'     4  \tab Mold/Protozoan/Coelenterate/Mycoplasma/Spiroplasma Mitochondrial \cr
#'     5  \tab Invertebrate Mitochondrial \cr
#'     6  \tab Ciliate/Dasycladacean/Hexamita Nuclear \cr
#'     9  \tab Echinoderm/Flatworm Mitochondrial \cr
#'     10 \tab Euplotid Nuclear \cr
#'     11 \tab Bacterial, Archaeal, and Plant Plastid \cr
#'     12 \tab Alternative Yeast Nuclear \cr
#'     13 \tab Ascidian Mitochondrial \cr
#'     14 \tab Alternative Flatworm Mitochondrial \cr
#'     15 \tab Blepharisma Macronuclear \cr
#'     16 \tab Chlorophycean Mitochondrial \cr
#'     21 \tab Trematode Mitochondrial \cr
#'     22 \tab Scenedesmus obliquus Mitochondrial \cr
#'     23 \tab Thraustochytrium Mitochondrial \cr
#'     24 \tab Pterobranchia Mitochondrial \cr
#'     25 \tab Candidate Division SR1 and Gracilibacteria \cr
#'     26 \tab Pachysolen tannophilus Nuclear
#'   }
#'
#' @param return Either `"count"` (default) to return the number of stop codons,
#'   or any other value to return the translated amino acid sequence.
#' @importFrom Biostrings getGeneticCode DNAString translate subseq
#' @importFrom stringr str_detect str_count
#' @return If `return = "count"`, an integer giving the number of stop codons.
#'   Otherwise, a character string with the translated amino acid sequence.
#'
#' @details The function uses [Biostrings::translate()] with the specified
#'   starting position and genetic code. Translation warnings are suppressed.
#'
#' @author Ramon Gallego, 2021
#' @examples
#' if (requireNamespace("Biostrings", quietly = TRUE)) {
#'   library(Biostrings)
#'   seq <- DNAString("ATGGCCATTGTAATGGGCCGCTGAAAGGGTGCCCGATAG")
#'   count_stop_codons(seq, codon = 1, dictionary = 1)
#'   count_stop_codons(seq, codon = 1, dictionary = 1, return = "translation")
#' }
#'
#' @export


count_stop_codons <- function(sequence = NULL, format = "DNAString", codon = 1, dictionary = 5, return = "count"){
  

 # Get the genetic Code
  code <- getGeneticCode(id_or_name2 = as.character(dictionary))
  
  if (format != "DNAString"){
    if(str_detect(sequence, "[^ACGT]")){
      stop("WARNING: Non ACGT characters found ")
    }
    input <- DNAString(sequence)} else { input <- sequence}
  
  translation <-  as.character(suppressWarnings(translate(subseq(input,start = codon),
                                                          genetic.code = code,
                                                          no.init.codon = T)))
  if(return != "count"){
    return(translation) } else {
  return(str_count(translation, "\\*"))}
                                                
}
