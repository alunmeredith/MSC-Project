library(stringr)
library(readr)
library(dplyr)
library(lubridate)
library(feather)

########### CONCATONATE CITATIONS FILES ##########################

files <- list.files("DataFiles/processed", recursive = T)
files <- str_subset(files, "citations(.*)\\.csv")
files.year <- as.numeric(substring(files, 1, 4))
files <- paste0("DataFiles/processed/", files)

pb <- txtProgressBar(min = 0, max = length(files), initial = 0, style = 3)
for (i in 1:length(files)) {
    citations <- read_csv(files[i], col_types = "ccc", progress = F)
    # Deal with descrepency in the name of patent (sometimes called patent_nos/patent_no)
    if ("parent" %in% names(citations)) {
        degree_file <- citations %>% group_by(parent) %>% summarise(citation_count = n())
    } else
    if ("patent_nos" %in% names(citations)) {
        degree_file <- citations %>% group_by(patent_nos) %>% summarise(citation_count = n())
    }
    print(paste0(i, "/", length(files)))
    gc()
}

write_csv(degree_file, "DataFiles/processed/degree_cat.csv", append = T)


########### CONCATONATE PATENT FILES ##########################

# List patent files
files <- list.files("DataFiles/processed", recursive = T)
files <- str_subset(files, "patent(.*)\\.csv")
files.year <- as.numeric(substring(files, 1, 4))
files <- paste0("DataFiles/processed/", files)

patents_cat <- NULL
pb <- txtProgressBar(min = 0, max = length(files), initial = 0, style = 3)
for (i in 1:length(files)) {
  setTxtProgressBar(pb, i)
  current <- read_csv(files[i])
  current$year <- files.year[i]
  # Converting date to POSIXct
  # Some of the dates have 00 for day (convert them to 01)
  current$pub_date <- str_replace(current$pub_date, "0000$", "0101")
  current$pub_date <- str_replace(current$pub_date, "00$", "01")
  current$pub_date <- ymd(current$pub_date)
  patents_cat <- bind_rows(patents_cat, current)
}
patents_cat$year <- as.factor(patents_cat$year)
# Add Degree of citations to patent
patents_cat <- full_join(patents_cat, citation, by = "patent_no")

write_csv(patents_cat, "DataFiles/processed/patent_cat.csv")
write_feather(patents_cat, "DataFiles/processed/patent_cat.feather")
