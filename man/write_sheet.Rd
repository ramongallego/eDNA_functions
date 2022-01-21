
\name{write_indexing_PCR}
\alias{write_idx}
\title{Fill Indexing PCRs}
\usage{
write_indexing_PCR(data, ss_name, template = "1naS-F_dj4SNmND5nJ5TKhMX3TikmRS00ILKjfg_Ucgc")
}
\description{
Fill a new spreadsheet with the samples (a dataframe with the columns Well, Sample and Column)
}
\examples{
indexing_ss <- "test_45"
data <- AMPURE

write_indexing_PCR(data, indexing_ss)
}
