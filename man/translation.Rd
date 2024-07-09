\name{translation}
\alias{count_stop_codons}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Count
}
\description{
Given a DNA sequence, either in DNAString format or as a character vector, 
calculate the number of stop codons found. If desired, it can return the translated sequence instead
}
\usage{
count_stop_codons(
  sequence = NULL,
  format = "DNAString",
  codon = 1,
  dictionary = 5,
  return = "count")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sequence}{
       A DNA sequence, either as DNAString or as a character vector
}
  \item{format}{
       The format in which the sequence is presented, either "DNAString" or  character vector
}
  \item{codon}{
       The position in the sequence where to start translating (usually 1,2 or 3, but you know your data)
}
  \item{dictionary}{
       Which translation code to use. Takes a number. See BioStrings::GENETIC_CODE_TABLE for all available options
 \tabular{ll}{
   \strong{id} \tab \strong{name} \cr
   1  \tab Standard                                                                                        \cr
   2  \tab Vertebrate Mitochondrial                                                                        \cr
   3  \tab Yeast Mitochondrial                                                                             \cr
   4  \tab Mold Mitochondrial; Protozoan Mitochondrial; Coelenterate Mitochondrial; Mycoplasma; Spiroplasma\cr
   5  \tab Invertebrate Mitochondrial                                                                      \cr
   6  \tab Ciliate Nuclear; Dasycladacean Nuclear; Hexamita Nuclear                                        \cr
   9  \tab Echinoderm Mitochondrial; Flatworm Mitochondrial                                                \cr
   10 \tab Euplotid Nuclear                                                                                \cr
   11 \tab Bacterial, Archaeal and Plant Plastid                                                           \cr
   12 \tab Alternative Yeast Nuclear                                                                       \cr
   13 \tab Ascidian Mitochondrial                                                                          \cr
   14 \tab Alternative Flatworm Mitochondrial                                                              \cr
   15 \tab Blepharisma Macronuclear                                                                        \cr
   16 \tab Chlorophycean Mitochondrial                                                                     \cr
   21 \tab Trematode Mitochondrial                                                                         \cr
   22 \tab Scenedesmus obliquus Mitochondrial                                                              \cr
   23 \tab Thraustochytrium Mitochondrial                                                                  \cr
   24 \tab Pterobranchia Mitochondrial                                                                     \cr
   25 \tab Candidate Division SR1 and Gracilibacteria                                                      \cr
   26 \tab Pachysolen tannophilus Nuclear                                                                  
 }
}
  \item{return}{
       the default is the number of stop codons, but if you change it it will return the actual AA sequence
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Ramon Gallego, 2021
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (x) 
{
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
