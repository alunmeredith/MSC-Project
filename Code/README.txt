Description of files

XML_analysis.Rmd (& .html)  ---   Initial analysis of the raw data: format, xml structure etc. 
Download_extract.R          ---   Programmatically downloads the entire dataset from bulkdata.uspto.gov
extract_basic.R             ---   Extracts the most basic elements from the dataset id/date of patents and id of their citations
build_adjacency_matrix.R    ---   Runs through extracted data to build a sparse matrix (list of interactions) between patents