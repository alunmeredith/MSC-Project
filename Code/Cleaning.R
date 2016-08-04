    # This script cleans the Processed data by:
        # Add column names, 
        # Removing duplicated entries, 
        # Concatonate patents into one df
        # Convert date to a date object, some of these are NA so original is retained
    
    # Note: there are parsing errors for the date, primarily this is when the date is written in the format "XXX   00"
    #       there are typically 0-10 of these per year, to avoid this date was initially read as a character
    
    # The script writes this to the "Cleaned" Directory
    
cleaning <- function(input.dir = "Processed", output.dir = "Cleaned",
                     patent.colnames = c("Patent", "Date", "Order"), patent.coltypes = "ccc",
                     citation.colnames = c("Patent", "Citation", "Date"), citation.coltypes = "ccc") {

    # Libraries
    library(readr)
    library(dplyr)
    library(lubridate)
    
    # Define input and output directories
    input.dir.pat <- paste0("../DataFiles/", input.dir, "/patent")
    output.dir.pat <- paste0("../DataFiles/", output.dir, "/patent")
    # Create directory to save data to
    dir.create(output.dir.pat, recursive = TRUE, showWarnings = FALSE)
    
    # Clean Patent files
    patent.files <- list.files(input.dir.pat, full.names = T)
    patent.cat <- NULL
    for (file in patent.files) {
        # Add Column Names 
        df <- read_csv(file, col_names = patent.colnames, col_types = patent.coltypes)
        # Replace duplicated entries 
        df <- unique(df)
        # Convert character date to a posixct class object
        df$Date2 <- ymd(df$Date)
        # If the year is 1976:2001 (txt files parsed) then remove a digit from the end of patent number
        yr <- stringr::str_extract(file, "[0-9]{4}.csv$") %>% substring(1,4) %>% as.numeric() # extract year
        if (yr %in% 1976:2001) {
            df$Patent <- sapply(df$Patent, function(x) substring(x, 1, nchar(x) - 1)) 
        }
        # Remove the 0s from text patent numbers
        type_txt <- stringr::str_extract(df$Patent, "^(D|RE|PP|H|T)")
        type_txt[is.na(type_txt)] <- ""
        rem0_txt <- sub("^(D|RE|PP|H|T)*0+", "", df$Patent)
        df$Patent <- paste0(type_txt, rem0_txt)
        # Concatonate Patents 
        patent.cat <- bind_rows(patent.cat, df)
        # Write to file
        file.path <- sub(input.dir, output.dir, file)
        write_csv(df, file.path)
    }
    write_csv(patent.cat, paste0("../DataFiles/", output.dir, "/patent_cat.csv"))
    
    # Clear environments
    rm(patent.cat, df)
    
    input.dir.cit <- paste0("../DataFiles/", input.dir, "/citation")
    output.dir.cit <- paste0("../DataFiles/", output.dir, "/citation")
    dir.create(output.dir.cit, recursive = TRUE, showWarnings = FALSE)
    
    # Clean Citation Files
    citation.files <- list.files(input.dir.cit, full.names = T)
    for (file in citation.files) {
        gc()
        print(file)
        # Add Column Names
        df <- read_csv(file, col_names = citation.colnames, col_types = citation.coltypes)
        # Remove Duplicated Entries
        df <- unique(df)
        # If the year is 1976:2001 (txt files parsed) then remove a digit from the end of patent number
        yr <- stringr::str_extract(file, "[0-9]{4}.csv$") %>% substring(1,4) %>% as.numeric() # extract year
        if (yr %in% 1976:2001) {
            df$Patent <- sapply(df$Patent, function(x) substring(x, 1, nchar(x) - 1)) 
        }
        # Remove the 0s from text patent numbers
        type_txt <- stringr::str_extract(df$Patent, "^(D|RE|PP|H|T)")
        type_txt[is.na(type_txt)] <- ""
        rem0_txt <- sub("^(D|RE|PP|H|T)*0+", "", df$Patent)
        df$Patent <- paste0(type_txt, rem0_txt)
        # To reduce size of file convert patent to factor
        df$Patent <- as.factor(df$Patent)
        # Convert character date to a posixct class object
        # Day not recorded "00" so replaced with "01" to be valid posixct objects
        df$Date2 <- gsub('.{2}$', '01', df$Date)
        df$Date2 <- ymd(df$Date2)
        # Write to file
        file.path <- sub(input.dir, output.dir, file)
        write_csv(df, file.path)
        rm(df)
    }
}
