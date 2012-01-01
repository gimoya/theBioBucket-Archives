# Filename: source_https.R
# Purpose: function to source raw code from github project
# Author: Tony Bryal
# Date: 2011-12-10
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-github/


source_https <- function(url, ...) {
 # load package
 require(RCurl)

 source_script <- function(u) {
   # read script lines from website using a security certificate
   script <- getURL(u, followlocation = TRUE,
                    cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
  
   # parse lines and evaluate in the global environement
   eval(parse(text = script), envir= .GlobalEnv)
 }
 
 # source each script
 sapply(c(url, ...), source_script)
}


# Example
# source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R",
#              "https://raw.github.com/gimoya/theBioBucket-Archives/master/R/Better_Word_Cloud.R",
#              "https://raw.github.com/gimoya/theBioBucket-Archives/master/R/GScholarScraper_2.R",
#              "https://raw.github.com/gimoya/theBioBucket-Archives/master/R/RegEx_Examples.R")