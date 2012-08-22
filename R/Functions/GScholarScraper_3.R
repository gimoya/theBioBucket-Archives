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

GScholar_Scraper <- function(input, write = F) {

    require(XML)

    # putting together the search-url:
    url <- paste("http://scholar.google.com/scholar?q=", input, "&num=1&as_sdt=1&as_vis=1", 
        sep = "")
    
    # get content and parse it:
    doc <- htmlParse(url)
    
    # number of hits:
    x <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
    y <- strsplit(x, " ")[[1]][2] 
    num <- as.integer(sub("[[:punct:]]", "", y))
    
    # If there are no results, stop and throw an error message:
    if (num == 0 | is.na(num)) {
        stop("\n\n...There is no result for the submitted search string!")
    }
    
    pages.max <- ceiling(num/100)
    
    # 'start' as used in url:
    start <- 100 * 1:pages.max - 100
    
    # Collect urls as list:
    urls <- paste("http://scholar.google.com/scholar?start=", start, "&q=", input, 
        "&num=100&as_sdt=1&as_vis=1", sep = "")
    
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
        
        dat <- data.frame(TITLES = tit, PUBLICATION = pub, LINKS = lin)
        return(dat)
    }

    result <- do.call("rbind", lapply(urls, scraper_internal))
    if (write == T) {
      result$LINKS <- paste("=Hyperlink(","\"", result$LINKS, "\"", ")", sep = "")
      write.table(result, "GScholar_Output.CSV", sep = ";", 
                  row.names = F, quote = F)
      shell.exec("GScholar_Output.CSV") 
      } else {
      return(result)
    }
}

input <- "allintitle:ziggy stardust"
GScholar_Scraper(input, write = T)

input <- "allintitle:live on mars"
GScholar_Scraper(input, write = F)

# ERROR with message: input <- "allintitle:crazyshit"; GScholar_Scraper(input)

