#' functions to translate mass into molarity and viceversa, given we are talking about double stranded DNA
#' it requires two inputs, the mass (or molarity) and the length of the amplicon
#' It works with the two most common concentrations used in Molecular Ecology labs
#'  ng/ul for mass
#'  nM for molarity

#'
#' @param ng Numeric. the concentration in ng per uL 
#' @param length_amplicon Integer. The length of the amplicon in bp.
#' 
#' 
#' @return Numeric. The equivalent concentration in nmoles per Litre
#' @export
#'
#' @examples
#' \dontrun{
#' data("molarity.data")
#' molarity.data |> mutate(moles = ng2nM(ng=mass, length_amplicon = Amp_len))
#' }


ng2nM <- function(ng, length_amplicon){
x <- (ng*1000000)/(660*length_amplicon)
return(x)
}

#' @param nM Numeric. The concentration in nmoles per Litre
#' @param length_amplicon Integer. The length of the amplicon in bp.
#' 
#' @return Numeric. The equivalent concentration in ng per uL 
#' @rdname ng2nM
#' @export
#'
#' @examples
#' \dontrun{
#' data("molarity.data")
#' molarity.data |> mutate(mass = nM2ng(nM=Molarity, length_amplicon = Amp_len))
#' }
#' 
nM2ng  <- function(nM, length_amplicon){
  x <- (nM*660*length_amplicon)/(1000000)
  return(x)
}


