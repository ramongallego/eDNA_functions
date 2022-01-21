\name{read_indexing_PCR}
\alias{read_idx}
\title{Parsing Indexing PCRs}
\usage{
read_indexing_PCR(ss)
}
\description{
When preparing MiSeq libraries using Nextera indices, I use a Google spreadsheet for adding the samples and the Indetifiers they use.

Use this function to recover all that info
}
\examples{
indexing_ss <- "1iJc7MRdQQH18UPt5tBS13D5ry_s69OZ1cLcD7dntZ3Q"

read_indexing_PCR(indexing_ss)
}
