# This function cycles through a directory (year) and parses it acording to the above two functions and saves them 
parse_directory <- function(year) {
    require(stringr)
    
    # Find the type of datafile based on year
    if (year %in% 2005:2016) {
        type <- "xml"
    } else 
    if (year %in% 2001:2004) {
        type <- "sgml"
    } else 
    if (year %in% 1976:2001) {
        type <- "dat"
    } else stop("year out of bounds")
    
    ##### DEFINITIONS ############
    # define read and write directories
    read.dir <- paste0("DataFiles/Raw/", year)
    write.dir <- paste0("DataFiles/Processed/", year)

    ##### LIST FILES #############
    # Read files in directory
    files <- list.files(read.dir)
    files <- str_subset(files, "^((?!cat).)*$")
    files.lst <- str_subset(files, "lst\\.txt$")
    if (length(files.lst) > 0) {
        lst.paths <- paste0(read.dir, "/", files.lst)
    }
    if (type %in% c("xml", "sgml")) {
        files <- str_subset(files, "\\.xml")
    } else 
    if (type == "dat") {
        files <- str_subset(files, "\\.dat")
    }
    # Read files which have already been processed
    files.processed <- list.files(paste0(write.dir, "/citations"))
    if (length(files.processed) > 0) {
        # Generate a regex expression for finding those files in the read directory
        print(sprintf("Files already processed: %i", length(files.processed)))
        files.processed <- str_replace(files.processed, "\\.csv", "")
        files.processed.regex <-  paste0(files.processed, "|", collapse = "")
        files.processed.regex <-  substr(files.processed.regex, 1, nchar(files.processed.regex)-1)
        # Exclude already processed files from the list
        files <- files[!str_detect(files, files.processed.regex)]
    }    
    files.root <- str_replace(files, "\\..{3}", "") 
    file.paths <- paste0(read.dir, "/", files)
    for (i in seq_along(files)) {
        print(sprintf("Processing: %s (%i/%i)", files.root[i], i, length(files)))
        t <- Sys.time()
        
        parsed <- parse_file(file.paths[i], year, type)
        
        print(paste("Time to process: ", Sys.time() - t))
        print(paste("Estimated time rematining:", (length(files) - i)*(Sys.time()-t)))

        # Save files
        print(paste("Writing: ", files.root[i]))
        dirs <- c(write.dir, paste0(write.dir, c("/citations", "/patent", "/misspat")))
        for (dir in dirs) {
            if (!dir.exists(dir)) dir.create(dir, recursive = T)
        }
        write_csv(as.data.frame(parsed$citations), paste0(write.dir, "/citations/", files.root[i], ".csv"))
        write_csv(as.data.frame(parsed$patents), paste0(write.dir, "/patent/", files.root[i], ".csv"))
        
        if (exists("lst.paths")) {
            patent.list <- read_lines(lst.paths[i])
            patents.missing <- patent.list[!(patent.list %in% parsed$patents$patent_no)]
            write_csv(as.data.frame(patents.missing), paste0(write.dir, "/misspat/", files.root[i], ".csv"))
        }
    }
}    

# This function reads in a data file and returns a list of citations for each patent document 
parse_file <- function(file.txt, year, type) {
    library(readr)
    # Read a file into R
    lines <- read_lines(file.txt)
    
    if (type == "dat") {parsed <- parse_dat(lines)}
    else {
        # Create an index to seperate the different xml documents within.
        xmltags <- grep("<?xml", lines, fixed = T)
        
        citations <- NULL
        patents <- NULL
        pb <- txtProgressBar(min = 0, max = length(xmltags), style = 3)
        # Loop over each xml document in the file
        for (i in 2:length(xmltags)) {
            # Display a progress bar
            setTxtProgressBar(pb, i)
            # Define the xml document 
            xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
            # Parse document
            if (type == "xml") {
                parsed.xml <- tryCatch(parse_xml(xml, year), error = function(e) {print(e); return(NA)})
            } 
            else if (type == "sgml") {
                parsed.xml <- tryCatch(parse_sgml(xml, year), error = function(e) {print(e); return(NA)})
            }
            if (anyNA(parsed.xml)) next 
            if (nrow(parsed.xml$cit_info) == 0) next
            # Link parent doc-id to citation df
            parsed.xml$cit_info <- cbind(parent = parsed.xml$patent_info$patent_no, parsed.xml$cit_info)
            # Bind citation df/patent df as one file
            citations <- tryCatch(rbind(citations, parsed.xml$cit_info), error = function(e) {print(e); return(citations)})
            patents <- tryCatch(rbind(patents, parsed.xml$patent_info), error = function(e) {print(e); return(patents)})
        }
        close(pb)
        parsed <- list(citations = citations, patents = patents)
    }
    gc()
    return(parsed)
}    

# Function to parse the older text based format
# TODO:: make flexible the variables being stored
parse_dat <- function(lines) {
    # initialise lists we want to record
    t <- Sys.time()
    patent_no <- NULL
    citation_patent_no <- list()
    citation_publish_date <- list()
    publish_date <- NULL
    patent_ii <- 0
    sector_cit <- F
    sector_pat <- F
    pb <- txtProgressBar(min = 0, max = length(lines), initial = 0, style = 3)
    for (i in 1:length(lines)) {
        line <- lines[i]
        if (i %% 1000 == 0) setTxtProgressBar(pb, i)
        if (is.na(line)) next()
        tag <- substr(line, 1, 4)
        contents <- function(line) {
            contents <- tryCatch(str_trim(substring(line, 5)), error = function(e) browser())
            return(contents)
        }
        # Reset after each new patent header
        if (tag == "PATN") {
            # currently writing patent information
            sector_pat <- TRUE
            sector_cit <- FALSE
            # Index the working position that will be written to
            patent_ii <- patent_ii + 1
            citation_ii <- 0
            # Initialise new lists for that information
            citation_patent_no[patent_ii] <- vector(mode = "character", length = 1)
            citation_publish_date[patent_ii] <- vector(mode = "character", length = 1)
        }
        # Add patent number 
        else if (tag == "WKU " & sector_pat) {
            patent_no[patent_ii] <- contents(line)
        }
        # Index the citation number for the start of a new reference
        else if (tag == "UREF") {
            # Currently writing to citation information
            sector_pat <- FALSE
            sector_cit <- TRUE
            # Index position of new citation
            citation_ii <- citation_ii + 1 
        }
        else if (tag == "PNO ") { # add referenced patent number
            citation_patent_no[[patent_ii]][citation_ii] <- contents(line)
        }
        else if (tag == "ISD " & sector_pat) { # Add publication dates for parent patents
            publish_date[patent_ii] <- contents(line)
        }
        else if (tag == "ISD " & sector_cit) { # Add publication dates for citations
            citation_publish_date[[patent_ii]][citation_ii] <- contents(line)
        }
    }
    close(pb)
    print(Sys.time() - t)
    
    # Convert the formats into the same as parse_file
    patent <- data.frame(patent_no = patent_no, pub_date = publish_date, stringsAsFactors = F)
    
    reps <- sapply(citation_patent_no, length)
    patent_nos <- rep(patent_no, reps)
    citation_no <- unlist(citation_patent_no)
    citation_pub_date <- unlist(citation_publish_date)
    if (length(citation_pub_date) != length(citation_no)) browser()
    citations <- data.frame(patent_nos, citation_no, citation_pub_date, stringsAsFactors = F)
    
    return(list(citations = citations, patents = patent))
}


# Function to parse the older text based format
# TODO:: make flexible the variables being stored
parse_sgml <- function(file.txt) {
    # Header type stuff
    library(readr)
    library(stringr)
    
    lines <- read_lines(file.txt)
    
    # initialise lists we want to record
    t <- Sys.time()
    patent_no <- NULL
    citation_patent_no <- list()
    citation_publish_date <- list()
    publish_date <- NULL
    patent_ii <- 0
    sector_cit <- F
    sector_pat <- F
    for (line in lines) {
        if (is.na(line)) next()
        tag <- substr(line, 1, 4)
        # Reset after each new patent header
        if (tag == "PATN") {
            # currently writing patent information
            sector_pat <- TRUE
            sector_cit <- FALSE
            # Index the working position that will be written to
            patent_ii <- patent_ii + 1
            citation_ii <- 0
            # Initialise new lists for that information
            citation_patent_no[patent_ii] <- vector(mode = "character", length = 1)
            citation_publish_date[patent_ii] <- vector(mode = "character", length = 1)
        }
        # Add patent number 
        else if (tag == "WKU " & sector_pat) {
            patent_no[patent_ii] <- str_trim(str_replace(line, "WKU", ""))
        }
        # Index the citation number for the start of a new reference
        else if (tag == "UREF") {
            # Currently writing to citation information
            sector_pat <- FALSE
            sector_cit <- TRUE
            # Index position of new citation
            citation_ii <- citation_ii + 1 
        }
        else if (tag == "PNO ") { # add referenced patent number
            citation_patent_no[[patent_ii]][citation_ii] <- str_trim(str_replace(line, "PNO", ""))
        }
        else if (tag == "ISD " & sector_pat) { # Add publication dates for parent patents
            publish_date[patent_ii] <- str_trim(str_replace(line, "ISD", ""))
        }
        else if (tag == "ISD " & sector_cit) { # Add publication dates for citations
            citation_publish_date[[patent_ii]][citation_ii] <- str_trim(str_replace(line, "ISD", ""))
        }
    }
    print(Sys.time() - t)
    
    # Convert the formats into the same as parse_file
    patent <- data.frame(patent_no = patent_no, pub_date = publish_date, stringsAsFactors = F)
    
    reps <- sapply(citation_patent_no, length)
    patent_nos <- rep(patent_no, reps)
    citation_no <- unlist(citation_patent_no)
    citation_pub_date <- unlist(citation_publish_date)
    citations <- data.frame(patent_nos, citation_no, citation_pub_date, stringsAsFactors = F)
    
    return(list(citations = citations, patents = patent))
}

# for (yr in 1987) {
#     parsed <- parse_dat(paste0("DataFiles/Raw/", yr, "/", yr, ".dat"))
#     write_csv(as.data.frame(parsed$citations), paste0("DataFiles/Processed/", yr, "_citations.csv"))
#     write_csv(as.data.frame(parsed$patents), paste0("DataFiles/Processed/", yr, "_patents.csv"))
#     gc()
# }

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




