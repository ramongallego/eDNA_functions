ng2nM <- function(ng, length_amplicon){
x <- (ng*1000000)/(660*length_amplicon)
return(x)
}
nM2ng  <- function(nM, length_amplicon){
  x <- (nM*660*length_amplicon)/(1000000)
  return(x)
}
