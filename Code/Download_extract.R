###
### This script programatically downloads the raw data from the uspto website 
### "https://bulkdata.uspto.gov/data2/patent/grant/redbook/bibliographic/"
###

download <- function(years = 2016:1976, download.to.dir = "DataFiles/Raw/") {
    library(RCurl)
    library(XML)
    download.urls <-
        sapply(years, 
               function(year) {
                    url <- paste0("https://bulkdata.uspto.gov/data2/patent/grant/redbook/bibliographic/", year, "/")
                    webpage <- getURL(url)
                    webpage <- readLines(tc <- textConnection(webpage)); close(tc)
                    pagetree <- htmlTreeParse(webpage, error=function(...){}, useInternalNodes = TRUE)
                
                    x <- xpathSApply(pagetree, "//*/tr", xmlValue)
                    x <- stringr::str_extract(x, ".*\\.zip") 
                    x <- na.omit(x)
                
                    return(paste0(url, x))
                    }
               )
    names(download.urls) <- paste0("Yr", years)

    # Download each file
    for (year in years) {
        download.location <- paste0(download.to.dir, year)
        if (!dir.exists(download.location)) dir.create(download.location, recursive = TRUE)
        urls.year <- download.urls[[paste0("Yr", year)]]
    
        # Remove from the download queue any files which  have already been downloaded
        already.downloaded <- list.files(download.location)
        if(length(already.downloaded) > 0) {
            already.downloaded <- sapply(already.downloaded, function(string) stringr::str_extract(string, ".*\\."))
            already.downloaded <- unique(already.downloaded)
            x <- sapply(already.downloaded, function(x) grepl(x, urls.year))
            urls.year <- urls.year[!rowSums(x)]
        }
    
        # Download each file and unzip them into folders based on yea
        for (url in urls.year) {
            temp <-  tempfile()
            try(download.file(url, temp))
            try(unzip(temp, exdir = paste0(download.to.dir, year)))
            unlink(temp)
        }
    }

    # Create a list of the files which were unable to be downloaded
    # Write that list to a file in the data directory
    urls.missed <- vector()
    for (year in years) {
        download.location <- paste0(download.to.dir, year)
        urls.year <- download.urls[[ paste0("Year_", year) ]]
    
        # Remove from the download queue any files which  have already been downloaded
        already.downloaded <- list.files(download.location)
        if(length(already.downloaded) > 0) {
            already.downloaded <- sapply(already.downloaded, function(string) stringr::str_extract(string, ".*\\."))
            already.downloaded <- unique(already.downloaded)
            x <- sapply(already.downloaded, function(x) grepl(x, urls.year))
            if (length(x) > 1) {
                urls.missed.year <- urls.year[!rowSums(x)]
                urls.missed <- c(urls.missed, urls.missed.year)
            }
        }
    }
    write(urls.missed, file = paste0(download.to.dir, "Unable_to_download.txt"))
}