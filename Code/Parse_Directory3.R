# This function cycles through a directory (year) and parses it acording to the above two functions and saves them 
parse_directory <- function(year) {
    require(stringr)
    source("Code/Parse_Directory_statebased.R")
    # Find the type of datafile based on year
    if (year %in% 2005:2016) {
        type <- "xml"
    } else 
    if (year %in% 2001:2004) {
        type <- "sgml"
    } else 
    if (year %in% 1976:2001) {
        type <- "text"
    } else stop("year out of bounds")
    
    ##### DEFINITIONS ############
    # define read and write directories
    read.dir <- paste0("DataFiles/Raw/", year)
    write.dir <- paste0("DataFiles/Processed/")
    
    ##### LIST FILES #############
    # Read files in directory
    files <- list.files(read.dir)
    temp <- str_subset(files, "cat")
    if (length(temp) > 0) files <- temp
    files.lst <- str_subset(files, "lst\\.txt$")
    if (length(files.lst) > 0) {
        lst.paths <- paste0(read.dir, "/", files.lst)
    }
    if (type %in% c("xml", "sgml")) {
        files <- str_subset(files, "\\.xml")
    } else 
    if (type == "text") {
        files <- str_subset(files, "\\.dat")
    }
    
    # Read files which have already been processed
    if (exists("FilesProcessed.txt")) {
        files.processed <- readLines("FilesProcessed.txt")
        # Generate a regex expression for finding those files in the read directory
        files.processed <- (files.processed %in% files)
        print(sprintf("Files already processed: %i", length(files.processed)))
        # Exclude already processed files from the list
        files <- files[!(files %in% files.processed)]
    }    
    
    files.root <- str_replace(files, "\\..{3}", "") 
    file.paths <- paste0(read.dir, "/", files)

    for (i in seq_along(files)) {
        print(sprintf("Processing: %s (%i/%i)", files.root[i], i, length(files)))
        t <- Sys.time()
        
        parse(input_path = file.paths[i], "text", output_path_patent = paste0(write.dir, "patent/" ,year, ".csv"),
              output_path_citation = paste0(write.dir, "citation/", year, ".csv"))
        
        print(paste("Time to process: ", Sys.time() - t))
        if (length(files) > 1) print(paste("Estimated time rematining:", (length(files) - i)*(Sys.time()-t)))
        
        if (exists("lst.paths")) {
            patent.list <- read_lines(lst.paths[i])
            patents.missing <- patent.list[!(patent.list %in% parsed$patents$patent_no)]
            write_csv(as.data.frame(patents.missing), paste0(write.dir, "/misspat/", files.root[i], ".csv"))
        }
    }
}    



