taxonomy <-  function (local.file = "", db = "NCBI", synonyms = FALSE) 
{
    if (!identical(db, "NCBI")) {
        stop("Only the NCBI taxonomy database is available in this version\n")
    }
    tmp <- tempdir()
    
    message("Extracting data\n")
    test <- untar(tarfile = local.file,
                  exdir = tmp)
    if (!identical(test, 0L)) 
        stop(cat(test))
    message("Building data frame\n")
    x <- scan(file = paste0(tmp, "/nodes.dmp"), what = "", 
        sep = "\n", quiet = TRUE)
    x <- strsplit(x, split = "\t")
    x <- sapply(x, function(s) s[c(1, 3, 5)])
    nodes <- as.data.frame(t(x), stringsAsFactors = FALSE)
    nodes[[1]] <- as.integer(nodes[[1]])
    nodes[[2]] <- as.integer(nodes[[2]])
    colnames(nodes) <- c("taxID", "parent_taxID", 
        "rank")
    x <- scan(file = paste0(tmp, "/names.dmp"), what = "", 
        sep = "\n", quiet = TRUE)
    if (synonyms) {
        syn <- x[grepl("synonym", x)]
        syn <- strsplit(syn, split = "\t")
        syn <- sapply(syn, function(s) s[c(1, 3)])
        syn <- as.data.frame(t(syn), stringsAsFactors = FALSE)
        syn[[1]] <- as.integer(syn[[1]])
        colnames(syn) <- c("taxID", "name")
    }
    x <- x[grepl("scientific name", x)]
    x <- strsplit(x, split = "\t")
    x <- sapply(x, function(s) s[c(1, 3)])
    namez <- as.data.frame(t(x), stringsAsFactors = FALSE)
    namez[[1]] <- as.integer(namez[[1]])
    colnames(namez) <- c("taxID", "name")
    taxa <- merge(nodes, namez, by = "taxID")
    taxa$parent_taxID[taxa$taxID == 1] <- 0
    if (synonyms) {
        taxapp <- taxa[match(syn$taxID, taxa$taxID), ]
        taxapp$name <- syn$name
        rownames(taxapp) <- NULL
        taxa <- rbind(taxa, taxapp)
    }
    message("Done\n")
    return(taxa)
}