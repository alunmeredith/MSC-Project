# Populate a sparse matrix
library(slam)
require(readr)
# Sparse matrices are stored in triplet format [i,j,value] (values are logical)

# INDEX ###################################################################################################
# To create this format we must create a vector of patents from which i/j are obtained. 
# Parse each file and find unique patents
# NB: at this point have to decide if we want to store only the patents which have been parsed or all cited patents
#     to make it smaller, do the former. The negative of this is to analyse involving those discarded patents we 
#     must 

index <- NULL
# List dirs to extract from
dirs <- "DataFiles/Processed/2016/patent"
for (dir in dirs) {
    files <- list.files(dir, full.names = T)
    for (file in files) {
        data <- read_csv(file)
        index <- unique(c(index, data$patent_no))
    }
}

write(index, "DataFiles/Processed/index.txt")

# VALUES ###################################################################################################
# To find the values for the sparse matrix we must match the index with cited patents in the citations files
index <- read("DataFiles/Processed/index.txt")
interactions <- NA
# Cycle through each file in directories given
dirs <- "DataFiles/Processed/2016/citations"
for (dir in dirs) {
    files <- list.files(dir, full.names = T)
    for (file in files) {
        data <- read_csv(file)
        matches <- match(index, data$patent_no) # Returns NA or position along data$patent_no where match occurs
        i <- which(!is.na(match(index, data$patent_no)))  # pointer along index of patent
        x <- data$parent[na.omit(match(index, data$patent_no))] -> x # values of parent which match index (j)
        j <- match(x, index) # pointer along index of parent
        interactions <- rbind(interactions, data.frame(i = i, j = j))
    }
}

# Build sparse matrix
adjacency_matrix <- simple_triplet_matrix(interactions$i, interactions$j, TRUE, ncol = length(index), dimnames = index)