# File-Name: GScholarScraper_3.R
# Date: 2012-08-22
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: Scrape Google Scholar search result
# Packages used: XML
# Licence: CC BY-SA-NC
#
# Arguments:
# (1) search_str:
# A search string as used in Google Scholar search dialog
#
# (2) write:
# Logical, should a table be writen to user default directory, by default hyperlinks to the publications are created!

GScholar_Scraper <- function(x, write = F) {

    require(XML)

    # putting together the search-url:
    url <- paste("http://scholar.google.at/scholar?q=", x, "&hl=en&lr=lang_en&num=1&as_sdt=1&as_vis=1", 
        sep = "")
    
    # get content and parse it:
    doc <- htmlParse(url)
    
    # number of hits:
    x <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
    num <- as.integer(strsplit(x, " ")[[1]][2])
    
    # If there are no results, stop and throw an error message:
    if (num == 0 | is.na(num)) {
        stop("\n\n...There is no result for the submitted search string!")
    }
    
    pages.max <- ceiling(num/100)
    
    # 'start' as used in url:
    start <- 100 * 1:pages.max - 100
    
    # Collect urls as list:
    urls <- paste("http://scholar.google.com/scholar?start=", start, "&q=", search_str, 
        "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1", sep = "")
    
    scraper_internal <- function(x) {
        
        doc <- htmlParse(x, encoding="UTF-8")
        
        # titles:
        tit <- xpathSApply(doc, "//h3[@class='gs_rt']", xmlValue)
        
        # publicated:
        pub <- xpathSApply(doc, "//div[@class='gs_a']", xmlValue)
        
        # links:
        lin <- xpathSApply(doc, "//h3[@class='gs_rt']/a", xmlAttrs)
        
        # summaries are truncated, and thus wont be used..  abst <- xpathSApply(doc,
        # '//div[@class='gs_rs']', xmlValue)

        # to be extended for individual needs..
        
        dat <- data.frame(TITLES = tit, PUBLICATION = pub, LINKS = lin)
        return(dat)
    }
    result <- do.call("rbind", lapply(urls, scraper_internal))
    if (write == T) {
      first <- sub(" .*", "", result$TITLES)
      result$LINKS <- as.character(result$LINKS)
      result$LINKS[first %in% c("[HTML]", "[PDF]")] <- paste("=Hyperlink(","\"", result$LINKS[first %in% c("[HTML]", "[PDF]")], "\"", ")", sep = "")
      write.table(result, "GScholar_Output.CSV", sep = ";", 
                  row.names = F, quote = F)
      shell.exec("GScholar_Output.CSV") 
      } else {
      return(result)
    }
}

search_str <- "allintitle:ziggy stardust"
GScholar_Scraper(search_str, write = F)

search_str <- "allintitle:live mars"
GScholar_Scraper(search_str, write = F)

# ERROR with message: search_str <- "allintitle:WHATTHEF"; GScholar_Scraper(search_str)

