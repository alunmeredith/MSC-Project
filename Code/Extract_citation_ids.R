
# This function parses a single xml document
##############################################################################################################
# Retrieves basic info of the patent that document is about, returned as a named list
# Retreives basic info about the patents cited by that document, returned as a matrix
# Documents any structural attributes which are not as expected (and therefore elements have been thrown away)
parse_xml <- function(doc.txt) {
    library(xml2)
    
    doc.xml <- read_xml(doc.txt)
    y <- xml_find_all(doc.xml, "//us-bibliographic-data-grant/publication-reference/document-id")
    patent.info <- as.character(xml_contents(xml_children(y)))
    names(patent.info) <- xml_name((xml_children(y)))
    parent.patent <- patent.info["doc-number"]
    
    citations <- xml_find_all(doc.xml, "//us-references-cited/us-citation/patcit/document-id")
    attr <- lapply(citations, function(x) xml_name(xml_children(x)))       
    values <- lapply(citations, function(x) as.character(xml_contents(xml_children(x))))
    
    # Build a matrix from the return values where they are correctly named
    # If incorrectly named, report the name and values
    allowed.attr <- c("country", "doc-number", "kind", "name", "date")
    citation.info <- matrix(NA, ncol = length(values), nrow = 5, dimnames = list(allowed.attr, NULL))
    
    for (i in seq_along(attr)) {
        attr.i <- attr[[i]]
        values.i <- values[[i]]
        # Create an index relating the attributes to the columns of the matrix
        mat.index <- sapply(attr.i, function(x) {
            index <- which(allowed.attr == x)
            if (length(index) < 1) return(NA)
            return(index)
        })
        values.i <- values.i[!is.na(mat.index)]
        mat.index <- na.omit(mat.index)
        citation.info[mat.index,i] <- values.i
    }
    
    citation.info.old <- citation.info
    if (dim(citation.info)[2] > 0) {
        citation.info <- tryCatch(rbind(citation.info, parent.patent), warning = function(w) w)
    }
    else citation.info <- NULL
    if (is(citation.info, "warning")) browser()
    
    return(list(patent.info = patent.info, citation.info = t(citation.info)))
}


# This function reads in a data file and returns a list of citations for each patent document 
parse_file <- function(file.txt) {
    library(readr)
    # Read a file into R
    lines <- read_lines(file.txt)
    # Create an index to seperate the different xml documents within.
    xmltags <- grep("<?xml", lines, fixed = T)
    
    # Loop over each xml document in the file
    citation.ids.file <- list()
    citation.info.file <- NULL
    patent.info.file <- NULL
    #for (i in 2:length(xmltags)) {
    for (i in 2:200) { ######## TEST CODE, restricts number of files processed to 5 #############
        # Define the xml document 
        xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
        
        parsed.xml <- try(parse_xml(xml), silent = T)

        # Error catching in case no citations returned by parse_xml
        if (class(parsed.xml) !=  "try-error") {
            citaiton.info.file.old <- citation.info.file
            citation.info.file <- tryCatch(rbind(citation.info.file, parsed.xml$citation.info), error = function(e) e)
            if (is(citation.info.file, "error")) browser()
            patent.info.file <- rbind(patent.info.file, parsed.xml$patent.info)
        }
    }
    gc()
    return(list(citations = citation.info.file, patent = patent.info.file))
}


# This function cycles through a directory (year) and parses it acording to the above two functions and saves them 
parse_directory <- function(year) {
    read.dir <- paste0("DataFiles/Raw/", year)
    write.dir <- paste0("DataFiles/Processed/", year)
    
    # Define the list of files which needss to be processed
    # Read files in directory
    files <- list.files(read.dir)
    require(stringr)
    files.xml <- str_subset(files, "\\.xml")
    # Read files which have already been processed
    files.processed <- list.files(write.dir)
    if (length(files.processed) > 0) {
        # Generate a regex expression for finding those files in the read directory
        files.processed <- str_replace(files.processed, "_(incorrectattr|misspat|patent|citations)\\.csv", "")
        print(sprintf("Files already processed: %i", length(files.processed)/4))
        files.processed.regex <-  paste0(files.processed, "|", collapse = "")
        files.processed.regex <-  substr(files.processed.regex, 1, nchar(files.processed.regex)-1)
        # Exclude already processed files from the list
        files.xml <- files.xml[!str_detect(files.xml, files.processed.regex)]
    }    
    
    files.root <- str_replace(files.xml, "\\.xml", "")    
    file.paths.xml <- paste0(read.dir, "/", files.xml)
    file.paths.txt <- paste0(read.dir, "/", files.root, "lst.txt")
    for (i in seq_along(files.xml)) {
        print(sprintf("Processing: %s (%i/%i)", files.root[i], i, length(files.xml)))
        t <- Sys.time()
        parsed <- parse_file(file.paths.xml[i])
        print(paste("Time to process: ", Sys.time() - t))
        print(paste("Estimated time rematining:", (length(files.xml) - i - 1)*(Sys.time()-t)))
        patent.list <- read_lines(file.paths.txt[i])
        patents.missing <- patent.list[!(patent.list %in% parsed$patent[,2])]
        
        # Save files
        print(paste("Writing: ", files.root[i]))
        if (!dir.exists(write.dir)) dir.create(write.dir, recursive = T)
        write_csv(as.data.frame(parsed$citations), paste0(write.dir, "/", files.root[i], "_citations.csv"))
        write_csv(as.data.frame(parsed$patent), paste0(write.dir, "/", files.root[i], "_patent.csv"))
        write_csv(as.data.frame(patents.missing), paste0(write.dir, "/", files.root[i], "_misspat.csv"))
        gc()
    }
}

