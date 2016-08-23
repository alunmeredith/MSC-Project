setwd("D:/Documents/Github/Southampton/Project/DataFiles/Cleaned/citation")

library(readr)
library(dplyr)
library(data.table)
library(rbenchmark)
library(lubridate)
files <- list.files()

# Load patents into memory and set a key for Patent so its easily searched
patents <- read_csv("../patent_cat.csv")
patents <- arrange(patents, Date)
patents$CitationTime <- cumsum(patents$Order)
patents <- data.table(patents)
setkey(patents, Patent)

# Read a citation file 
file <- read_csv(files[1])
file <- file %>% arrange(Date2)

# Lookup patent for each citation and add its date
file[,c("PatentDate","PatentTime")] <- 
    t(sapply(file$Patent, function(x) {
    res <- patents[.(x)]
    c(res[["Date"]], res[["CitationTime"]])
}))

# Lookup citation (in patents table) for each citation and add its CitationTime
file[,"CitationTime"] <- 
    t(sapply(file$Citation, function(x) {
        res <- patents[.(x)]
        res[["CitationTime"]]
    }))

# Calculate days difference between the two
file <- mutate(file, PatentDate2 = (ymd(PatentDate)), diffDays = PatentDate2 - Date2)
 