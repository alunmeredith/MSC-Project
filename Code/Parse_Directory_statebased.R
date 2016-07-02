lines <- read_lines(file.txt)
lines_ii <- 1
length_ii <- length(ii)

parse_patent <- function(lines_ii) {
    bib <- rep(NA, 2)
    cit <- rep(NA, 3)
    
    line <- lines[lines_ii]
    tag <- extract_tag(line, type)
    
    state <- patent_end(tag, state)
    
    if ("PATENT" %IN% state) {
        parsed <- parse_patent(lines_ii + 1)
    } else {
        return(list(bib, cit))
    }
}

extract_tag <- function(line, type) {
    if (type == "text") {
        tag <- substring(line, 1, 4)
    } else
    if (type == "sgml") {
        tag <- NA
    } else 
    if (type == "xml") {
        tag <- NA
    }
    
    return(tag)
}

patent_end <- function(tag, state, type) {
    if (lines_ii == length_ii) {
        state <- state[state != "PATN"]
    }
    if (type == "text") {
        if (tag == "PATN") {
            state <-  state[state != "PATN"]
        }
    } else
    if (type == "sgml") {
        state <- state
    } else 
    if (type == "xml") {
        state <- state
    }
}