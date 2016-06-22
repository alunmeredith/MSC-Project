source("Code/Extract_citation_ids.R")

# Test xml parsing for each year to see results #########################################
years <- 2016:2004

# Extract file paths for a sample file for each year
dirs <- paste0("DataFiles/Raw/", years)
require(stringr)
paths <- sapply(dirs, function(dir) {
    x <- list.files(dir)
    x <- str_subset(x, ".xml")[4]
    x <- paste0(dir, "/", x)
    return(x)
    })

# Parse each sample file
test_years <- list()
for (i in seq_along(paths)) {
    test_years[[paste("yr", years[i])]] <- parse_file(paths[i])
}


# Extract for a specific year ###########################################################
read.dir <- "DataFiles/Raw/2015"
write.dir <- "DataFiles/Processed/2015"
parse_directory <- parse_directory(read.dir, write.dir)


# Merge results for a year ##############################################################
