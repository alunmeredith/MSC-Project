# 
# This script aims to read the raw xml data files and extract a list of doc.ids which correspond to cited patents
#

### NB:: Some kind of memory leak is present when running this code, some initial investigations suggest this may be due to the XML package
### This memory leak is documented here: http://stackoverflow.com/questions/23696391/memory-leak-when-using-package-xml-on-windows
### I have the code shown and is still an issue, therefore have to use a different XML parsing package

# This function uses the xml node structure of 2016 to find the citation ids from a single xml document    
parse_xml <- function(doc.txt) {
    library(XML)
    doc.xml <- try(xmlTreeParse(doc.txt, asText = T, useInternalNodes = T), silent = T)
    if (class(doc.xml) ==  "try-error") stop("Could not parse xml")
    root <- xpathSApply(doc.xml, "//us-bibliographic-data-grant/publication-reference/document-id", xmlValue)
    # Error catching for if more than one patent document is within 
    if (length(root) > 1) {
        print("more than one patent document")
        return(NA)
    }
    citation.ids <- xpathSApply(doc.xml, "//us-references-cited/us-citation/patcit/document-id/doc-number", xmlValue)
    return(list(root,citation.ids))
}

# This function reads in a data file and returns a list of citations for each patent document 
parse_file <- function(file.txt) {
    # Read a file into R
    lines <- readLines(file.txt)
    # Create an index to seperate the different xml documents within.
    xmltags <- grep("<?xml", lines)

    # Loop over each xml document in the file
    citation.ids.file <- list()
    for (i in 2:length(xmltags)) {
        # Define the xml document 
        xml <- paste(lines[xmltags[i-1]:(xmltags[i]-1)], collapse = "")
        citation.ids.doc <- try(parse_xml(xml), silent = T)
        # Error catching in case no citations returned by parse_xml
        if(length(class(citation.ids.doc)) > 1) print(class(citation.ids.doc))
        if (class(citation.ids.doc) !=  "try-error") {
            citation.ids.file[[ citation.ids.doc[[1]] ]] <- citation.ids.doc[[2]]
        }
    }
    gc()
    return(citation.ids.file)
}

#
citation.ids <- list()
data.files <- list.files("DataFiles/Raw/2015")
data.files <- data.files[grepl("\\.xml$", data.files)]
for (file in data.files[6:10]){
    print(paste("Processing: ", file))
    #save(citation.ids.file, file = "DataFiles/Citation.ids/2015/file")
    citation.ids <- c(citation.ids, parse_file("DataFiles/Raw/2015/ipgb20150106.xml"))
    showMemoryUse(decreasing = T, limit = 3)
    gc()
}