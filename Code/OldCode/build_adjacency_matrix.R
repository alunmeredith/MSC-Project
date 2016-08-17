library(data.table)
library(dplyr)
library(readr)

# Load vector of patents into memory, this will be indexed against to produce adjacency matrix. 
# Using data.table package to produce a vector key for binary search. 

pat_index <- read_csv("../DataFiles/Cleaned/patent_cat.csv")
pat_index <- data.table(pat_index, key = c("Patent", "Date"))

find_adjacency <- function(Pat_no, index = pat_index) {
    pat <- index[.(Pat_no), nomatch = 0]
    if (nrow(pat) > 0) {
        return(c(as.character(pat$Patent), as.character(Pat_no)))
    }
    else return(c(NA,as.character(Pat_no)))
}


adjacency_parse_file <- function(file = "../DataFiles/Cleaned/citation/1996.csv", write.file = "../DataFiles/Cleaned/adjacency.csv") {
    cursor = 0
    while (!is.null(cit_df <- read_csv(file, skip = cursor, n_max = 10000, col_types = "ccdD", 
                                       col_names = c("Patent", "Citation", "Date", "Date2")))) {
        print(cursor)
        adjacency <- lapply(cit_df$Citation, find_adjacency) %>% t 
        adjacency <- as.data.frame(matrix(unlist(adjacency), ncol = 2, byrow = TRUE))
        colnames(adjacency) <- c("Patent", "Citation")
        print(head(adjacency,3))
        write_csv(adjacency, write.file, append = TRUE)
        cursor <- cursor + 10000
    }
}

file.list <- list.files("../DataFiles/Cleaned/citation/", full.names = TRUE)
for(file in file.list) {
    print(file)
    adjacency_parse_file(file)
}