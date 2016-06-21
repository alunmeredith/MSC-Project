### Script to produce very basic patent id lists
# This scipt primarily amends the parse_xml function so that it is more flexible as to which features to extract
# Unfortunately this currently is at a significant cost to the time taken to parse each file.

# Define lookup tables of xml paths to extract based on year
Xpath_tbl_patent <- data.frame(Year = 2016:2002,
                    patent_no = "//us-bibliographic-data-grant/publication-reference/document-id/doc-number",
                    pub_date = "//us-bibliographic-data-grant/publication-reference/document-id/date",
                    stringsAsFactors = F)

Xpath_tbl_cit <- data.frame(Year = 2016:2002,
                            Roots = "//us-references-cited/us-citation/patcit/document-id",
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
    
    # Loop over each xml document in the file
    citations <- NULL
    patents <- NULL
    #for (i in 2:length(xmltags)) {
    for (i in 2:200) { ######## TEST CODE, restricts number of files processed to 5 #############
        # Define the xml document 
        xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
        # Parse document
        parsed.xml <- tryCatch(parse_xml(xml, year), error = function(e) {print(e); return(NA)})
        if (anyNA(parsed.xml)) next
        # Link parent doc-id to citation df
        parsed.xml$cit_info <- cbind(parent = parsed.xml$patent_info$patent_no, parsed.xml$cit_info)
    
        # Bind citation df/patent df as one file
        citations <- tryCatch(rbind(citations, parsed.xml$cit_info), error = function(e) {print(e); return(citations)})
        patents <- tryCatch(rbind(patents, parsed.xml$patent_info), error = function(e) {print(e); return(patents)})
        
        gc()
    }
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
    
    files.xml <- files.root[1:5] ######## TEST CODE, restricts number of files processed to 5 #############
    
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
        write_csv(as.data.frame(parsed$patents), paste0(write.dir, "/", files.root[i], "_patent.csv"))
        write_csv(as.data.frame(patents.missing), paste0(write.dir, "/", files.root[i], "_misspat.csv"))
        gc()
    }
}    
    
    
    

# doc.txt <- "DataFiles/sample_2016_doc.xml"
# lines <- read_lines(doc.txt)
# doc.txt <- paste(lines, collapse = "")
# parse_xml(doc.txt, 2016)

time <- Sys.time()
file.txt <- "DataFiles/sample_2016_file.xml"
t <- parse_file(file.txt, 2016)
time - Sys.time()