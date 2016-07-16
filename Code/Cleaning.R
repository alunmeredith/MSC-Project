# This script cleans the Processed data by:
    # Add column names, 
    # Removing duplicated entries, 
    # Concatonate patents into one df
    # Convert date to a date object, some of these are NA so original is retained

# Note: there are parsing errors for the date, primarily this is when the date is written in the format "XXX   00"
#       there are typically 0-10 of these per year, to avoid this date was initially read as a character

# The script writes this to the "Cleaned" Directory

# Libraries
library(readr)
library(dplyr)
library(lubridate)

# Create directory to save data to
dir.create("DataFiles/Cleaned/patent", recursive = TRUE, showWarnings = FALSE)
dir.create("DataFiles/Cleaned/citation", recursive = TRUE, showWarnings = FALSE)

# Clean Patent files
patent.files <- list.files("DataFiles/Processed/patent", full.names = T)
patent.cat <- NULL
for (file in patent.files) {
    # Add Column Names 
    df <- read_csv(file, col_names = c("Patent", "Date", "Order"))
    # Replace duplicated entries 
    df <- unique(df)
    # Convert character date to a posixct class object
    df$Date2 <- ymd(df$Date)
    # Concatonate Patents 
    patent.cat <- bind_rows(patent.cat, df)
    # Write to file
    file.path <- sub("Processed", "Cleaned", file)
    write_csv(df, file.path)
}
write_csv(patent.cat, "DataFiles/Cleaned/patent_cat.csv")

# Clear environments
rm(list = ls())

# Clean Citation Files
citation.files <- list.files("DataFiles/Processed/citation", full.names = T)
citation.files <- tail(citation.files, 2)
for (file in citation.files) {
    print(file)
    # Add Column Names
    df <- read_csv(file, col_names = c("Patent", "Citation", "Date"), col_types = "ccc")
    # Remove Duplicated Entries
    df <- unique(df)
    # To reduce size of file convert patent to factor
    df$Patent <- as.factor(df$Patent)
    # Convert character date to a posixct class object
    # Day not recorded "00" so replaced with "01" to be valid posixct objects
    df$Date2 <- gsub('.{2}$', '01', df$Date)
    df$Date2 <- ymd(df$Date2)
    # Write to file
    file.path <- sub("Processed", "Cleaned", file)
    write_csv(df, file.path)
}

