# This function cycles through a directory (year) and parses it acording to the above two functions and saves them 
parse_directory <- function(year, write.dir = "../DataFiles/Processed2",  prefer.cat = TRUE, doc_type = NULL) {
    require(stringr)
    #source("Parse_File_statebased.R")
    
    dirs <- paste0(write.dir, c("/patent","/citation"))
    sapply(dirs, function(dir) if(!dir.exists(dir)) {dir.create(dir, recursive = TRUE)})

    # Find the doc_type of datafile based on year
    if (is.null(doc_type)) {
        if (year %in% 2005:2016) {
            doc_type <- "xml"
        } else 
        if (year %in% 2002:2004) {
            doc_type <- "sgml"
        } else 
        if (year %in% 1976:2001) {
            doc_type <- "text"
        } else stop("year out of bounds")
    }
    
    ##### DEFINITIONS ############
    # define read directories
    read.dir <- paste0("../DataFiles/Raw/", year)

    ##### LIST FILES #############
    # Read files in directory
    files <- list.files(read.dir)

    temp <- str_subset(files, "cat")
    if (prefer.cat == FALSE) {
        files <- files[!(files %in% temp)]
    }
    if (length(temp) > 0 & doc_type != "text" & prefer.cat == TRUE) files <- temp
    files.lst <- str_subset(files, "lst\\.txt$")
    if (length(files.lst) > 0) {
        lst.paths <- paste0(read.dir, "/", files.lst)
    }
    if (doc_type %in% c("xml", "sgml")) {
        files <- str_subset(files, "(\\.xml)|(\\.sgml)")
    } else 
    if (doc_type == "text") {
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
    print(files)
    for (i in seq_along(files)) {
        print(sprintf("Processing: %s (%i/%i)", files.root[i], i, length(files)))
        t <- Sys.time()
        
        out_pat <-  paste0(write.dir, "/patent/" ,year, ".csv")
        out_cit <- paste0(write.dir, "/citation/", year, ".csv")
        parse(input_path = file.paths[i], type = doc_type, output_path_patent = out_pat, output_path_citation = out_cit)
        
        print(paste("Time to process: ", Sys.time() - t))
        if (length(files) > 1) print(paste("Estimated time rematining:", (length(files) - i)*(Sys.time()-t)))
        
        # if (exists("lst.paths")) {
        #     patent.list <- read_lines(lst.paths[i])
        #     patents.missing <- patent.list[!(patent.list %in% parsed$patents$patent_no)]
        #     write_csv(as.data.frame(patents.missing), paste0(write.dir, "/misspat/", files.root[i], ".csv"))
        # }
    }
}    



