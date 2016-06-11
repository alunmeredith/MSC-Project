
# This function parses a single xml document
##############################################################################################################
# Retrieves basic info of the patent that document is about, returned as a named list
# Retreives basic info about the patents cited by that document, returned as a matrix
# Documents any structural attributes which are not as expected (and therefore elements have been thrown away)
parse_xml <- function(doc.txt) {
    
    doc.xml <- read_xml(doc.txt)
    y <- xml_find_all(doc.xml, "//us-bibliographic-data-grant/publication-reference/document-id")
    patent.info <- as.character(xml_contents(xml_children(y)))
    names(patent.info) <- xml_name((xml_children(y)))
    
    citations <- xml_find_all(doc.xml, "//us-references-cited/us-citation/patcit/document-id")
    attr <- lapply(citations, function(x) xml_name(xml_children(x)))       
    values <- lapply(citations, function(x) as.character(xml_contents(xml_children(x))))
    
    # Build a matrix from the return values where they are correctly named
    # If incorrectly named, report the name and values
    allowed.attr <- c("country", "doc-number", "kind", "name", "date")
    citation.info <- matrix(NA, ncol = length(values), nrow = 5, dimnames = list(allowed.attr, NULL))

    #### TODO
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
        citation.info[mat.index,1] <- values.i
    }
    
    parent.patent <- patent.info["doc-number"]
    citation.info <- rbind(citation.info, parent.patent)
    
    incorrect.attr <- attr[attr != allowed.attr]
    if (length(incorrect.attr) > 1) incorrect.attr <- cbind(incorrect.attr, parent.patent)
    
    return(list(patent.info = patent.info, citation.info = t(citation.info), incorrect.attr = incorrect.attr))
}



# This function reads in a data file and returns a list of citations for each patent document 
parse_file <- function(file.txt) {
    # Read a file into R
    lines <- readLines(file.txt)
    # Create an index to seperate the different xml documents within.
    xmltags <- grep("<?xml", lines)
    
    # Loop over each xml document in the file
    citation.ids.file <- list()
    citation.info.file <- NA
    patent.info.file <- NA
    incorrect.attr.file <- NA
    for (i in 2:2){ #length(xmltags)) {
        # Define the xml document 
        xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
        
        parsed.xml <- try(parse_xml(xml), silent = T)
        # Error catching in case no citations returned by parse_xml
        if (class(parsed.xml) !=  "try-error") {
            citation.info.file <- rbind(citation.info.file, parsed.xml$citation.info)
            patent.info.file <- rbind(patent.info.file, parsed.xml$patent.info)
            incorrect.attr.file <- rbind(incorrect.attr.file, parsed.xml$incorrect.attr)
        }
    }
    gc()
    return(list(citations = citation.info.file, patent = patent.info.file, incorrect.attr = incorrect.attr.file))
}

test <- parse_file("DataFiles/testdata.xml")
# parse_xml(xml1)
