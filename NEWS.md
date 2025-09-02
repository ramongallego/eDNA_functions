# eDNAfuns (development version)

* Added internal helper `.tidy_PCR_blocks()` to handle repeated blocks of columns 
  in PCR spreadsheets and return tidy tibbles.
* Refactored `read_step1_PCR()` and `read_indexing_PCR()` to use `.tidy_PCR_blocks()`.
* This change is only in the development branch (GitHub) and not yet on CRAN.

# eDNAfuns 0.1.0

* Initial CRAN submission.
