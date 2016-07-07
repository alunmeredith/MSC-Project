source("Code/OldCode/parse_file_text.R")
source("Code/Parse_Directory_statebased.R")

### Test TEXT ######################################

test <- parse_file_txt("DataFiles/sample_2001_file.txt")

test_patent <- test$patents
test_citation <- test$citations

parse(input_path  = "DataFiles/Sample_2001_file.txt", type = "text", output_path_patent = "test.csv", output_path_citation = "test2.csv")
test2_patent <- read_csv("test.csv", col_names = c("Patent", "Date", "Degree"))
test2_citation <- read_csv("test2.csv", col_names = c("Parent", "Date", "Citation"))
test2_citation <- test2_citation[,c(1,3,2)] # Reorder to match test_citation
    
# Compare 
nrow(test2_patent)
nrow(test_patent)

nrow(test_citation)
nrow(test2_citation)

# There is one duplicate in both (so likely a recording error)
sum(duplicated(test2_citation))
sum(duplicated(test_citation))

# Compare rows 
# Define function
fun.12 <- function(x.1,x.2,...){
    x.1p <- do.call("paste", x.1)
    x.2p <- do.call("paste", x.2)
    x.1[! x.1p %in% x.2p, ]
}

# There are 64 objects in t not in t2, these are patents with no citations
t <- fun.12(test_citation, test2_citation)
# There are 6526 objects in t2 not in t, these are the foreign patents 
t2 <- fun.12(test2_citation, test_citation)
# Both scripts parse the exact same patents
t3 <- sum(!(test_patent$patent_no %in% test2_patent$Patent))
t3 <- sum(!(test2_patent$Patent %in% test_patent$patent_no))



#### TEST XGML ##############################

test2_patent <- read_csv("t.csv", col_names = c("Patent", "Date", "Degree"))
test2_citation <- read_csv("t2.csv", col_names = c("Parent", "Date", "Citation"))
test2_citation <- test2_citation[,c(1,3,2)] # Reorder to match test_citation

patent_list <- readLines("DataFiles/Raw/2003/pgb20030114lst.txt")

# All patent in the patent list document were parsed
nrow(test2_patent)
length(patent_list)


#### TEST CAT FILES ################################
cat <- readLines("DataFiles/Raw/2003/2003cat_lst.txt")

files <- list.files("DataFiles/Raw/2003", full.names = T)
files <- stringr::str_subset(files, "lst")
files <- files[!grepl("cat", files)]

combined <- NULL
for (file in files) {
    combined <- c(combined, readLines(file))
}

# Conclusion: cat files have some patents (52 in this case) which the other files do not 
# but not the other way around, therefore better to use the cat file if present. 