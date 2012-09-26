# Purpose: Get geographic coordinates for a given IP-address
# Author: Kay Cichini
# Date: 2011-12-18
# Output: A string holding longitude and latitude with format "X;Y"

IPtoXY <- function(x) {
   URL_IP <- paste("http://www.datasciencetoolkit.org//ip2coordinates/",
                   x, sep = "")
   api_return <- readLines(URL_IP, warn = F)
   lon1 <- api_return[grep("longitude", api_return)]
   lon <- gsub("[^[:digit:].]", "", lon1)
   lat1 <- api_return[grep("latitude", api_return)]
   lat <- gsub("[^[:digit:].]", "", lat1)
   return(paste(lat, lon, sep = ";"))
}

# Example:
IPtoXY("74.88.200.52")