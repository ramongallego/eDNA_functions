\name{tally_wide}
\alias{hello}
\title{Contingency Tables}
\usage{
tally_wide(cols = var1, rows = var2, wt = NULL)
}
\description{
Given a long table with more than one factors, it creates a contingency table of the number of cases that fall under each combination of  levels between factors 1 and 2.
It displays a wide table (i.e Human readable)
}
\examples{
data <- tibble (a = rep(LETTERS[1:4], each = 4),
b = rep(letters[1:2], 8))
}
