# R/globals.R
# Declare global variables to satisfy R CMD check
# These are mostly column names created or used in dplyr pipelines
# No objects are created in the user session

utils::globalVariables(c(
  # Columns created in eDNAindex() pipelines
  "sumreads", "Tot", "Row.prop", "nreps", 
  "mean.prop", "Colmax", "Normalized.reads",
  
  # Common columns used in your functions
  "Sample", "Hash", "nReads", "Well", "Set", "Barcode",
  "Column", "Colmax", "Limits", "Notes", "Row.prop",
  "Step", "Success", "Tot", "temp", "time_secs", "nreps", "sumreads",

  
  # Any other symbols flagged by check
  "n", "mean.prop"
))
