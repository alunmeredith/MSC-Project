# Populate a sparse matrix
library(slam)
require(readr)
library(data.table)
# Sparse matrices are stored in triplet format [i,j,value] (values are logical)

# INDEX ###################################################################################################
# To create this format we must create a vector of patents from which i/j are obtained. 
# Parse each file and find unique patents
# NB: at this point have to decide if we want to store only the patents which have been parsed or all cited patents
#     to make it smaller, do the former. The negative of this is to analyse involving those discarded patents we 
#     must 

index <- NULL
# List dirs to extract from
dirs <- c("DataFiles/Processed/2015/patent", "DataFiles/Processed/2014/patent", "DataFiles/Processed/2012/patent")
for (dir in dirs) {
    files <- list.files(dir, full.names = T)
    for (file in files) {
        data <- read_csv(file)
        index <- unique(c(index, data$patent_no))
    }
}

write(index, "DataFiles/Processed/index.txt")

t <- Sys.time()
# VALUES ###################################################################################################
# To find the values for the sparse matrix we must match the index with cited patents in the citations files
# TODO: store values as the date citation was made. 
index <- scan(file = "DataFiles/Processed/index.txt", what = "character")
#index <- as.data.table(index)
interactions <- NULL
# Cycle through each file in directories given
dirs <- c("DataFiles/Processed/2015/citations", "DataFiles/Processed/2014/citations", "DataFiles/Processed/2012/citations")
for (dir in dirs) {
    files <- list.files(dir, full.names = T)
    for (file in files) {
        data <- read_csv(file)
        matches <- chmatch(index, data$patent_no) # Returns NA or position along data$patent_no where match occurs
        i <- which(!is.na(chmatch(index, data$patent_no)))  # pointer along index of patent
        x <- data$parent[na.omit(chmatch(index, data$patent_no))] -> x # values of parent which match index (j)
        j <- chmatch(x, index) # pointer along index of parent
        interactions <- rbind(interactions, data.frame(patent = i, parent = j))
    }
}
Sys.time() - t
# Build sparse matrix
adjacency_matrix <- Matrix::sparseMatrix(interactions$i, interactions$j)
