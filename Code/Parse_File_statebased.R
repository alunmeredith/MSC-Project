parse <- function(input_path, type, output_path_patent, output_path_citation) {
    #browser()
    # Libraries 
    require(stringr)
    require(readr)
    
    # Save environment to reference
    Env <- environment()
    
    # Create output paths if they don't exist
    dirs <- str_replace_all(c(output_path_patent, output_path_citation), "/.+.csv$", "")
    sapply(dirs, function(dir) {
        if (!dir.exists(dir)) dir.create(dir, recursive = T)
    })
    
    # Actions ##########################################################
    
    # Initialise output vecotrs for patent / citations upon reaching a new patent
    initialise_result <- function() {
        # If files don't exist write their column names
        if (!file.exists(output_path_patent)) {
            write(patent_colnames, output_path_patent, sep = ",", append = F, ncolumns = length(patent_colnames))
        }
        if (!file.exists(output_path_citation)) {
            write(citation_colnames, output_path_citation, sep = ",", append = F, ncolumns = length(citation_colnames))
        }
        patent <<- vector("character", length = length(patent_colnames))
        citations <<- NULL
        citation_current <<- vector("character", length = length(citation_colnames) - 1)
    }
    
    # Write patent / citation information to file upon finding the end of the current patent
    flush_patent <- function() {
        if (patent[1] != "") {
            # Add degree to dataframe
            nrow_citations <- nrow(citations)
            if (is.null(nrow_citations)) nrow_citations <- 0
            patent[which(patent_colnames == "Order")] <- nrow_citations
            write(patent, output_path_patent, sep = ",", 
                  append = T, ncolumns = length(patent_colnames))
            write_csv(as.data.frame(citations), output_path_citation, append = T)
        }
    }
    
    # Add current citation to citations matrix after finding the end of the current citaiton
    flush_citation <- function() {
        if (sum(citation_current != rep("", length(citation_colnames) - 1)) > 0) {
            citations <<- rbind(citations, c(patent[1], citation_current))
            citation_current <<- vector("character", length = length(citation_colnames) - 1)
        }
    }
    
    # Upon finding relevant information add to patent/citation output
    add_patent_information <- function(tag = line_tag, State = state, 
                                       vars = tag_add_patent_information) {
        if (State != "patent") return()
        ii <- match(tag, vars)
        patent[ii] <<- contents()
    }
    
    add_citation_information <- function(tag = line_tag, State = state, 
                                         vars = tag_add_citation_information) {
        if (State != "citation") return()
        ii <- match(tag, vars)
        citation_current[ii] <<- contents()
    }
    
    # State change: upon reaching different sections change state 
    state_change_patent <- function() {
        state <<- "patent"
    }
    state_change_citation <- function() {
        state <<- "citation"
    }
    state_change_none <- function() {
        state <<- "none"
    }
    
    # Function to get contents (so not evaluated every line) 
    contents <- function(line = Env$line, type = Env$type) {
        ifelse(type == "text",
               str_trim(substring(line, 5)),
               str_replace_all(line, "<.*?>", ""))
    } 
    
    # If sgml check for citation and country information hidden inside the tag
    sgml_exceptions <- function(line) {
        if (grepl("(<CITED-BY-OTHER/>)|(<CITED-BY-OTHER>)", line)) {
            citation_current[which(citation_colnames == "Cited by") - 1] <<- "cited by other"    
        } else if (grepl("(<CITED-BY-EXAMINER/>)|(<CITED-BY-EXAMINER>)", line)){
            citation_current[which(citation_colnames == "Cited by") - 1] <<- "cited by examiner"    
        }
        if (grepl("<PARTY-US>", line)) {
            citation_current[which(citation_colnames == "Country") - 1] <<- "US"
        }
        # Remove some extra tags which create duplicate matches
        line <- str_replace(line, "(</DOC>)|(<CITED-BY-OTHER/>)|(<CITED-BY-EXAMINER/>)|(<PARTY-US>)|(<CITED-BY-OTHER>)|(<CITED-BY-EXAMINER>)", "")
        return(line)
    }
    
    ## initialisation ###################################################
    state <- "patent"
    # List the tags associated with each action function
    switch(type,
           text = {
               tag_initialise_result <- "PATN"
               tag_flush_patent <- "PATN"
               tag_flush_citation <- c("UREF", "FREF", "OREF", "DRWD", "PATN")
               tag_add_patent_information <- c("WKU ", "ISD ")
               tag_add_citation_information <- c( "PNO ", "ISD ")
               tag_state_change_citation <- c("UREF", "FREF")
               tag_state_change_patent <- "PATN"
               tag_state_change_none <- c("INVT","CLAS")
           },
           xml = {
               tag_initialise_result <- '<?xml version="1.0" encoding="UTF-8"?>'
               tag_flush_patent <- '</us-patent-grant>'
               tag_flush_citation <- c("</us-citation>", "</citation>")
               tag_add_patent_information <- c("<doc-number></doc-number>", "<date></date>",
                                               "<main-classification></main-classification>", 
                                               "<further-classification></further-classification>")
               tag_add_citation_information <- c("<doc-number></doc-number>", "<date></date>", 
                                                 "<category></category>", "<country></country>")
               tag_state_change_citation <- c("<us-citation>", "<citation>")
               tag_state_change_patent <- c("<publication-reference>", "<classification-national>")
               tag_state_change_none <- c("</publication-reference>", "</us-ciation>", 
                                          "</citation>", "</classification-national>")
               citation_colnames <- c("Patent", "Citation", "Date", "CitedBy", "Country")
               patent_colnames <- c("Patent", "Date", "MainClassification", "FurtherClassification", "Order")
           },
           sgml = {
               tag_initialise_result <- '<SDOBI>'
               tag_flush_patent <- "</SDOBI>"
               tag_flush_citation <- "</B561>"
               tag_add_patent_information <- c("<B110><DNUM><PDAT></PDAT></DNUM></B110>", # patent number
                                               "<B140><DATE><PDAT></PDAT></DATE></B140>", # Date
                                               "<B521><PDAT></PDAT></B521>", # Main classification,
                                               "<B522><PDAT></PDAT></B522>") # Further classification
               tag_add_citation_information <- c("<DOC><DNUM><PDAT></PDAT></DNUM>", # Patent number
                                                 "<DATE><PDAT></PDAT></DATE>", # Patent Date
                                                 "<CTRY><PDAT></PDAT></CTRY>") # Country
               tag_state_change_citation <- "<B561>"
               tag_state_change_patent <- '<SDOBI>'
               tag_state_change_none <- NULL
               citation_colnames <- c("Patent", "Citation", "Date", "CitedBy", "Country")
               patent_colnames <- c("Patent", "Date", "MainClassification", "FurtherClassification", "Order")
           })
    
    # Initialise total taglist and response index
    tag_list <- list(tag_flush_citation, tag_flush_patent, tag_state_change_patent, 
                     tag_state_change_citation, tag_state_change_none, tag_initialise_result,
                     tag_add_citation_information, tag_add_patent_information)
    tag_all <- unlist(tag_list)
    tag_index <- rep(1:length(tag_list), sapply(tag_list, length))
    # Vectorise action functions in same order as tags which call them
    action_functions <- list(flush_citation, flush_patent,  state_change_patent, 
                             state_change_citation, state_change_none, initialise_result,
                             add_citation_information, add_patent_information)
    
    # Read data
    text <- read_lines(input_path)
    
    ## LOOP ############################################################
    pb <- txtProgressBar(min = 0, max = length(text), initial = 0, style = 3)
    initialise_result()
    for (i in seq_along(text)) {
        line <- text[i]
        if (i %% 1000 == 0) setTxtProgressBar(pb, i)
        
        # Turn current line into a tag (acts differently for text / sgml)
        line_tag <- ifelse(type == "text", 
                           substring(line, 1, 4), 
                           str_replace_all(line, ">.*?<", "><")
                           )
        
        if (type == "sgml") line_tag <- sgml_exceptions(line_tag)
    
        # Check if it matches any in the list 
        matches <- tag_index[tag_all %in% line_tag]
        
        # If match occurs invoke the appropriate action function
        if (length(matches) > 0) {
            lapply(matches, function(x) action_functions[[x]]())
        }
    }
    if (type == "text") flush_patent() #Because text doesn't have close tags
    close(pb)
}


