# File-Name: GScholarScraper_3.2.R
# Date: 2013-07-11
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
# if TRUE ("T") a CSV-file will be created.
#
# Difference to version 3:
# (3) added "since" argument - define year since when publications should be returned..
# defaults to 1900..
#
# (4) added "citation" argument - logical, if "1" citations are included
# defaults to "0" and no citations will be included..
# added field "YEAR" to output 
#
# Caveat: if a submitted search string gives more than 1000 hits there seem
# to be some problems (I guess I'm being stopped by Google for roboting the site..)
#
# And, there is an issue with this error message:
# > Error in htmlParse(URL): 
# > error in creating parser for http://scholar.google.com/scholar?q
# I haven't figured out his one yet.. most likely also a Google blocking mechanism..
# Reconnecting / new IP-address helps..


GScholar_Scraper <- function(input, since = 1900, write = F, citation = 0) {

    require(XML)
    
    # flip values because the url uses 0 for inclusion of citations 
    citation <- ifelse(citation == 1, 0, 1)

    # putting together the search-URL:
    URL <- paste0("http://scholar.google.com/scholar?q=", input, "&as_sdt=1,5&as_vis=", 
                 citation, "&as_ylo=", since)
    cat("\nThe URL used is: ", "\n----\n", paste0("* ", "http://scholar.google.com/scholar?q=", input, "&as_sdt=1,5&as_vis=", 
                 citation, "&as_ylo=", since, " *"))
    
    # get content and parse it:
    doc <- htmlParse(URL)
    
    # number of hits:
    h1 <- xpathSApply(doc, "//div[@id='gs_ab_md']", xmlValue)
    h2 <- unlist(strsplit(h1, "\\s"))
    # in splitted string it is the second element which contains digits,
    # grab it and remove decimal signs and convert to integer
    num <- as.integer(gsub("[[:punct:]]", "", h2[grep("\\d", h2)[1]]))
    cat("\n\nNumber of hits: ", num, "\n----\n", "If this number is far from the returned results\nsomething might have gone wrong..\n\n", sep = "")
    
    # If there are no results, stop and throw an error message:
    if (num == 0 | is.na(num)) {
        stop("\n\n...There is no result for the submitted search string!")
    }
    
    pages.max <- ceiling(num/20)
    
    # 'start' as used in URL:
    start <- 20 * 1:pages.max - 20
    
    # Collect URLs as list:
    URLs <- paste("http://scholar.google.com/scholar?start=", start, "&q=", input, 
                  "&num=20&as_sdt=1,5&as_vis=", citation, "&as_ylo=", since, sep = "")
    
    scraper_internal <- function(URL) {
        
        doc <- htmlParse(URL, encoding="UTF-8")
        
        # titles:
        tit <- xpathSApply(doc, "//h3[@class='gs_rt']", xmlValue)
        
        # publication:
        pub <- xpathSApply(doc, "//div[@class='gs_a']", xmlValue)
        
        # summaries are truncated, and thus wont be used..  
        # abst <- xpathSApply(doc, '//div[@class='gs_rs']', xmlValue)
        # ..to be extended for individual needs
        options(warn=(-1))
        dat <- data.frame(TITLES = tit, PUBLICATION = pub, 
                          YEAR = as.integer(gsub(".*\\s(\\d{4})\\s.*", "\\1", pub)))
        options(warn=0)
        return(dat)
    }

    result <- do.call("rbind", lapply(URLs, scraper_internal))
    if (write == T) {
      write.table(result, "GScholar_Output.CSV", sep = ";", 
                  row.names = F, quote = F)
      shell.exec("GScholar_Output.CSV") 
      } else {
      return(result)
    }
}

# EXAMPLES:
# 1:
input <- "intitle:metapopulation"
df <- GScholar_Scraper(input, since = 1980, citation = 0)
nrow(df)
hist(df$YEAR, xlab = "Year", 
     main = "Frequency of Publications with\n\"METAPOPULATION\" in Title")

# 2:
input <- "allintitle:life on mars"
GScholar_Scraper(input, since = 2006, citation = 0)

# 3:
input <- "allintitle:ziggy stardust"
GScholar_Scraper(input, write = T)

# 4: ERROR with message:
input <- "allintitle:alien plants restoration"
GScholar_Scraper(input)

# 5: CAVEAT, Google blocks automated requests at about the 1000th hit:
input <- "metapopulation"
df <- GScholar_Scraper(input, since = 1980)
nrow(df)

# 6: this also leads to this error for example no. 1,
# because when including citations (.., citation = 1) 1000 hits are exceeded, 
# Google blocks and dataframe generation is not working..
input <- "intitle:metapopulation"
df <- GScholar_Scraper(input, since = 1980, citation = 1)
