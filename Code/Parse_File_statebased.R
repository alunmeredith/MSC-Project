parse <- function(input_path, type, output_path_patent, output_path_citation) {
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
        patent <<- vector("character", length = length(tag_add_patent_information))
        citations <<- NULL
        citation_current <<- vector("character", length = length(tag_add_citation_information))
    }
    
    # Write patent / citation information to file upon finding the end of the current patent
    flush_patent <- function() {
        if (patent[1] != "") {
            # Add degree to dataframe
            nrow_citations <- nrow(citations)
            if (is.null(nrow_citations)) nrow_citations <- 0
            patent <- c(patent, nrow_citations)
            
            write(patent, output_path_patent, sep = ",", 
                  append = T, ncolumns = length(tag_add_patent_information) + 1)
            write_csv(as.data.frame(citations), output_path_citation, append = T)
        }
    }
    
    # Add current citation to citations matrix after finding the end of the current citaiton
    flush_citation <- function() {
        if (sum(citation_current != c("","", "")) > 0) {
            citations <<- rbind(citations, c(patent[1], citation_current))
            citation_current <<- vector("character", length = length(tag_add_citation_information))
        }
    }
    
    # Upon finding relevant information add to patent/citation output
    add_patent_information <- function(tag = line_tag, State = state, 
                                       vars = tag_add_patent_information) {
        if (State != "patent") return()
        i <- match(tag, vars)
        patent[i] <<- contents()
    }
    
    add_citation_information <- function(tag = line_tag, State = state, 
                                         vars = tag_add_citation_information) {
        if (State != "citation") return()
        i <- match(tag, vars)
        citation_current[i] <<- contents()
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
               tag_add_patent_information <- c("<doc-number></doc-number>", "<date></date>")
               tag_add_citation_information <- c("<doc-number></doc-number>", "<date></date>")
               tag_state_change_citation <- c("<us-citation>", "<citation>")
               tag_state_change_patent <- "<publication-reference>"
               tag_state_change_none <- c("</publication-reference>", "</us-ciation>", "</citation>")
           },
           sgml = {
               tag_initialise_result <- '<SDOBI>'
               tag_flush_patent <- "</SDOBI>"
               tag_flush_citation <- "</B561>"
               tag_add_patent_information <- c("<B110><DNUM><PDAT></PDAT></DNUM></B110>",
                                               "<B140><DATE><PDAT></PDAT></DATE></B140>")
               tag_add_citation_information <- c("<DOC><DNUM><PDAT></PDAT></DNUM>", 
                                                 "<DATE><PDAT></PDAT></DATE>",
                                                 "<DATE><PDAT></PDAT></DATE></DOC>")
               tag_state_change_citation <- "<B561>"
               tag_state_change_patent <- '<SDOBI>'
               tag_state_change_none <- NULL
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


