### Script to produce very basic patent id lists
# This scipt primarily amends the parse_xml function so that it is more flexible as to which features to extract
# In terms of processing time, the citation loop consumes the most.
# There is some error occuring (possibly randomly) with the C code implemented in xml2. This causes crashes and 
# means that R must be restarted but doesn't get stuck at the same point. 

# Define lookup tables of xml paths to extract based on year
Xpath_tbl_patent <- data.frame(Year = 2016:2004,
                    patent_no = "//us-bibliographic-data-grant/publication-reference/document-id/doc-number",
                    pub_date = "//us-bibliographic-data-grant/publication-reference/document-id/date",
                    stringsAsFactors = F)

Xpath_tbl_cit <- data.frame(Year = 2016:2004,
                            Roots = c(rep("//us-references-cited/us-citation/patcit/document-id",4), #2016-2013
                            rep("//references-cited/citation/patcit/document-id", 9)), #2012-10
                            patent_no = "doc-number",
                            #name = "name",
                            date = "date",
                            stringsAsFactors = F)

parse_xml <- function(doc.txt, yr, XPATH_tbl_patent = Xpath_tbl_patent, XPATH_tbl_cit = Xpath_tbl_cit) {
    library(xml2)
    library(dplyr)
    doc.xml <- read_xml(doc.txt)
    
    # Build a list of patent information
    XPATH_ls <- filter(XPATH_tbl_patent, Year == yr) %>% select(-Year) # Produces a list of XPATHS/variables
    patent_info <- XPATH_ls
    for (i in seq_along(XPATH_ls)) {
        patent_info[i] <- tryCatch(xml_find_all(doc.xml, XPATH_ls[[i]]) %>% xml_contents() %>% as.character(),
                                   # warning = function(w) NA,
                                   error = function(e) {print(e); return(NA)})
    }
    
    # Build a list of citation information
    XPATH_ls <- filter(XPATH_tbl_cit, Year == yr) %>% select(-Year) # Produces a list of XPATHS/variables
    Roots <- xml_find_all(doc.xml, XPATH_ls$Roots)
    XPATH_ls <- select(XPATH_ls, -Roots)
    
    cit_info <- matrix(NA, nrow = length(Roots), ncol = length(XPATH_ls))
    colnames(cit_info) <- names(XPATH_ls)
    
    # This is the primary place refactoring can improve processing speed as xml_find_one is looped through many times
    for (i in seq_along(XPATH_ls)) {
        cit_info[,i] <- sapply(Roots, function(x) {
            tryCatch({xml_find_one(x, XPATH_ls[[i]]) %>% xml_contents() %>% as.character()},
                     error = function(e) NA)
        })
    }
    return(list(patent_info = patent_info, cit_info = cit_info))
}

# This function reads in a data file and returns a list of citations for each patent document 
parse_file <- function(file.txt, year) {
    library(readr)
    # Read a file into R
    lines <- read_lines(file.txt)
    # Create an index to seperate the different xml documents within.
    xmltags <- grep("<?xml", lines, fixed = T)
    
    citations <- NULL
    patents <- NULL
    pb <- txtProgressBar(min = 0, max = length(xmltags), style = 3)
    # Loop over each xml document in the file
    for (i in 2:length(xmltags)) {
    #for (i in 1610:length(xmltags)) { ######## TEST CODE, restricts number of files processed to 5 #############
        #browser()
        # Display a progress bar
        setTxtProgressBar(pb, i)
        # Define the xml document 
        xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
        # Parse document
        parsed.xml <- tryCatch(parse_xml(xml, year), error = function(e) {print(e); return(NA)})
        if (anyNA(parsed.xml)) next 
        if (nrow(parsed.xml$cit_info) == 0) next
        # Link parent doc-id to citation df
        parsed.xml$cit_info <- cbind(parent = parsed.xml$patent_info$patent_no, parsed.xml$cit_info)
        # Bind citation df/patent df as one file
        citations <- tryCatch(rbind(citations, parsed.xml$cit_info), error = function(e) {print(e); return(citations)})
        patents <- tryCatch(rbind(patents, parsed.xml$patent_info), error = function(e) {print(e); return(patents)})
    }
    close(pb)
    gc()
    return(list(citations = citations, patents = patents))
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
    files.xml <- str_subset(files.xml, "^((?!cat).)*$")
    # Read files which have already been processed
    files.processed <- list.files(paste0(write.dir, "/citations"))
    if (length(files.processed) > 0) {
        # Generate a regex expression for finding those files in the read directory
        print(sprintf("Files already processed: %i", length(files.processed)))
        files.processed <- str_replace(files.processed, "\\.csv", "")
        files.processed.regex <-  paste0(files.processed, "|", collapse = "")
        files.processed.regex <-  substr(files.processed.regex, 1, nchar(files.processed.regex)-1)
        # Exclude already processed files from the list
        files.xml <- files.xml[!str_detect(files.xml, files.processed.regex)]
    }    
    
    #files.xml <- files.xml[1:5] ######## TEST CODE, restricts number of files processed to 5 #############
    
    files.root <- str_replace(files.xml, "\\.xml", "")    
    file.paths.xml <- paste0(read.dir, "/", files.xml)
    file.paths.txt <- paste0(read.dir, "/", files.root, "lst.txt")
    for (i in seq_along(files.xml)) {
        print(sprintf("Processing: %s (%i/%i)", files.root[i], i, length(files.xml)))
        t <- Sys.time()
        parsed <- parse_file(file.paths.xml[i], year)
        print(paste("Time to process: ", Sys.time() - t))
        print(paste("Estimated time rematining:", (length(files.xml) - i)*(Sys.time()-t)))
        patent.list <- read_lines(file.paths.txt[i])
        patents.missing <- patent.list[!(patent.list %in% parsed$patents$patent_no)]
        
        # Save files
        print(paste("Writing: ", files.root[i]))
        dirs <- c(write.dir, paste0(write.dir, c("/citations", "/patent", "/misspat")))
        for (dir in dirs) {
            if (!dir.exists(dir)) dir.create(dir, recursive = T)
        }
        write_csv(as.data.frame(parsed$citations), paste0(write.dir, "/citations/", files.root[i], ".csv"))
        write_csv(as.data.frame(parsed$patents), paste0(write.dir, "/patent/", files.root[i], ".csv"))
        write_csv(as.data.frame(patents.missing), paste0(write.dir, "/misspat/", files.root[i], ".csv"))
    }
}    
# Test Code ##############

## Test parse_xml
# doc.txt <- "DataFiles/sample_2016_doc.xml"
# lines <- read_lines(doc.txt)
# doc.txt <- paste(lines, collapse = "")
# parse_xml(doc.txt, 2016)

# # Test parse_file
# time <- Sys.time()
# file.txt <- "DataFiles/Raw/2013/ipgb20130101.xml"
# t <- parse_file(file.txt, 2013)
# print(Sys.time() - time)

# Test parse_directory
for (i in 2013) {
    parse_directory(i)
}
#2007