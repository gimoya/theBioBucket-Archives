# Filename: source_https.R
# Purpose: function to source raw code from github project
# Author: Tony Bryal
# http://tonybreyal.wordpress.com/2011/11/24/source_https-sourcing-an-r-script-from-
# github/
#
# CAINFO Edits: Kay Cichini
# Date: 2011-12-10


source_https <- function(u) {
 # load package
 require(RCurl)

 # read script lines from website using a security certificate
 CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
 script <- getURL(u, followlocation = TRUE, cainfo = CAINFO)

 # parse lines and evealuate in the global environement
 eval(parse(text = script), envir= .GlobalEnv)
}

source_https("https://raw.github.com/tonybreyal/Blog-Reference-Functions/master/R/bingSearchXScraper/bingSearchXScraper.R")

