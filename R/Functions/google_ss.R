# Script name: google_ss.R
# Purpose: Read spreadsheet data from Google Docs to R workspace
# Used packages: RCurl
# Author: Kay Cichini
# Date: 12-03-2012

google_ss <- function(gid = NA, key = NA)
    {
    if (is.na(gid)) {stop("\nWorksheetnumber (gid) is missing\n")}
    if (is.na(key)) {stop("\nDocumentkey (key) is missing\n")}
    require(RCurl)
    url <- getURL(paste("https://docs.google.com/spreadsheet/pub?key=", key,
                        "&single=true&gid=", gid, "&output=csv", sep = ""),
                  cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
    read.csv(textConnection(url), header = T, sep = ",")
    }
 
## Example:
## Mind that the worksheets are numbered consecutively from 0 to n,
## irrespective of the actual worksheet-name.
## The key should be put in apostrophes.
## The URL works only for published spreadsheets!
 
(data <- google_ss(gid = 0,
                   key = "0AmwAunwURQNsdDNpZzJqTU90cmpTU0sza2xLTW9fenc"))