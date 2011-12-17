# Purpose: get geographic coordinates for a given IP-address
# Author: Kay Cichini
# Date: 2011-12-18

IPtoXY <- function(x) { 
   URL_IP <- paste("http://www.datasciencetoolkit.org//ip2coordinates/",
                   x, sep = "")
   api_return <- readLines(URL_IP, warn = F)
   str_elements <- gsub("[^[:alnum:].]", "",
                        strsplit(api_return, "\"")[[1]])
   lon <- str_elements[grep("longitude", str_elements) + 1]
   lat <- str_elements[grep("latitude", str_elements) + 1]
   return(paste(lon, lat, sep = ";"))
}

# Example:
IPtoXY("74.88.200.52")