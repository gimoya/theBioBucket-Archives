GoogleHits <- function(input)
   {
    require(XML)
    require(RCurl)
    url <- paste("https://www.google.com/search?q=\"",
                 input, "\"", sep = "")
 
    CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
    script <- getURL(url, followlocation = TRUE, cainfo = CAINFO)
    doc <- htmlParse(script)
    res <- xpathSApply(doc, '//*/div[@id="resultStats"]', xmlValue)
    cat(paste("\nYour Search URL:\n", url, "\n", sep = ""))
    cat("\nNo. of Hits:\n")
    return(as.integer(gsub("[^0-9]", "", res)))
   }
 
# Example:
GoogleHits("thebiobucket")