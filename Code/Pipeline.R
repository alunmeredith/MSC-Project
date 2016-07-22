
# Download Data -----------------------------------------------------------

source("Download_extract.R")


# Extract Data  -----------------------------------------------------------
# Parses through each set of files in the same manner to extract the date and
# patent numbers of each patent and citation (posted to seperate files), 
# also computes order of each patent. 

source("Parse_File_statebased.R")
source("Parse_Directory3.R")

for(yr in 1976:2015) {
    ifelse(yr %in% c(2005,2006), pref <- FALSE, pref <- TRUE)
    parse_directory(yr, prefer.cat = pref)
}


# Clean Data --------------------------------------------------------------
# Names columns, Removes Duplicated Values, Concatonates patent data into one df
source("Cleaning.R")

