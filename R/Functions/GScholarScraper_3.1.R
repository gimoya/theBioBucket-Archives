# File-Name: GScholarScraper_3.R
# Date: 2012-08-22
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: Scrape Google Scholar search result
# Packages used: XML
# Licence: CC BY-SA-NC
#
# Arguments:
# (1) input:
# A search string as used in Google Scholar search dialog
#
# (2) write:
# Logical, should a table be writen to user default directory?
# if TRUE a CSV-file with hyperlinks to the publications will be created.
#
# Caveat: if a submitted search string gives more than 1000 hits there seem
# to be some problems (I guess I'm being stopped by Google for roboting the site..)
#
# Difference to version 3:
# added "since" argument - define year since when publications should be returned.. 
# added field "YEAR" to output

GScholar_Scraper <- function(input, since = 1990, write = F) {

    require(XML)
    require(stringr)

    # putting together the search-URL:
    URL <- paste("http://scholar.google.com/scholar?q=", input, "&num=1&as_sdt=1&as_vis=1", 
                 "&as_ylo=", since, sep = "")
    
    # get content and parse it:
    doc <- htmlParse(URL)
    
    # number of hits:
    h1 <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
    h2 <- strsplit(h1, " ")[[1]][2] 
    num <- as.integer(sub("[[:punct:]]", "", h2))
    
    # If there are no results, stop and throw an error message:
    if (num == 0 | is.na(num)) {
        stop("\n\n...There is no result for the submitted search string!")
    }
    
    pages.max <- ceiling(num/100)
    
    # 'start' as used in URL:
    start <- 100 * 1:pages.max - 100
    
    # Collect URLs as list:
    URLs <- paste("http://scholar.google.com/scholar?start=", start, "&q=", input, 
                  "&num=100&as_sdt=1&as_vis=1", "&as_ylo=", since, sep = "")
    
    scraper_internal <- function(x) {
        
        doc <- htmlParse(x, encoding="UTF-8")
        
        # titles:
        tit <- xpathSApply(doc, "//h3[@class='gs_rt']", xmlValue)
        
        # publication:
        pub <- xpathSApply(doc, "//div[@class='gs_a']", xmlValue)
        
        # links:
        lin <- xpathSApply(doc, "//h3[@class='gs_rt']/a", xmlAttrs)
        
        # summaries are truncated, and thus wont be used..  
        # abst <- xpathSApply(doc, '//div[@class='gs_rs']', xmlValue)
        # ..to be extended for individual needs
        
        dat <- data.frame(TITLES = tit, PUBLICATION = pub, 
                          YEAR = gsub("\\s", "", str_extract(pub, "\\s\\d{4}\\s")),
                          LINKS = lin)
        return(dat)
    }

    result <- do.call("rbind", lapply(URLs, scraper_internal))
    if (write == T) {
      result$LINKS <- paste("=Hyperlink(","\"", result$LINKS, "\"", ")", sep = "")
      write.table(result, "GScholar_Output.CSV", sep = ";", 
                  row.names = F, quote = F)
      shell.exec("GScholar_Output.CSV") 
      } else {
      return(result)
    }
}

# EXAMPLES:
input <- "allintitle:live on mars"
GScholar_Scraper(input, since = 2006)

input <- "allintitle:ziggy stardust"
GScholar_Scraper(input, write = T)

# ERROR with message: input <- "allintitle:crazyshit"; GScholar_Scraper(input)
