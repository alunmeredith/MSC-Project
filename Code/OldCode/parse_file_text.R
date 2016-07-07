# Function to parse the older text based format
# TODO:: make flexible the variables being stored
parse_file_txt <- function(file.txt) {
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
        contents <- str_trim(substring(line, 5))
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
            patent_no[patent_ii] <- contents
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
            citation_patent_no[[patent_ii]][citation_ii] <- contents
        }
        else if (tag == "ISD " & sector_pat) { # Add publication dates for parent patents
            publish_date[patent_ii] <- contents
        }
        else if (tag == "ISD " & sector_cit) { # Add publication dates for citations
            citation_publish_date[[patent_ii]][citation_ii] <- contents
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

# for (yr in 1990) {
#     parsed <- parse_file_txt(paste0("DataFiles/Raw/", yr, "/", yr, ".dat"))
#     write_csv(as.data.frame(parsed$citations), paste0("DataFiles/Processed/", yr, "_citations.csv"))
#     write_csv(as.data.frame(parsed$patents), paste0("DataFiles/Processed/", yr, "_patents.csv"))
#     gc()
# }
