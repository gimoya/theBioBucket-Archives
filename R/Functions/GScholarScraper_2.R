# File-Name: GScholarScraper_2.R
# Date: 2011-11-12
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: Extract and examine Google Scholar search result (publication titles)
# Packages used: RCurl, Rcpp, stringr, tm, wordcloud,
# Licence: CC BY-SA-NC
#
# Arguments:
#
# (1) search.str:
# A search string as used in Google Scholar search dialog
# (!) Important: use "+" between elements of the search string..
#
# (2) write.table:
# Logical, defining if a table holding category (book, article, pdf),
# full titles & links to publications should be saved to the default system folder.
#
# Output: a data frame with word frequencies (publication titles), optionally a
# CSV-file of the results, a word cloud
#
# Error reported: Error in substring(string, start, end) :
# invalid multibyte string at ' * Wi<6c>dlife
# may be resolved by: Sys.setlocale(locale="C")
#
# recent edits: 6-12-2011, resolved bug with no. of search results..

GScholarScraper <- function(search.str, write.table = FALSE){

require(Rcpp)
require(RCurl)
require(stringr)
require(tm)
require(wordcloud)

# Some explanations regarding the search string parameterization:
# "&lr?lang_en" will search only publications in English.
# "&num=100" will retur 100 results per site, strangely one yields different
# numbers of results when changing this parameter.. so I will use num = 100
# which will give me the largest number of results.
# "&as_vis=1" exculdes citations, in this version of the function I will
# exclude these because they may bias the final word frequencies
# due to the fact that citations often occurr multiply.
# "&hl_en" defines language used on site.
# "&as_sdt=1" returns only articles excluding patents.

# Get number of search results, making a first input to Google Scholar,
# retrieving results 1 to 100 from first result page, and containing the
# total no. of results somewhere:
url <- paste("http://scholar.google.com/scholar?start=0&q=",
      search.str, "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1",
      sep = "")

# ...i�m using urls like: http://scholar.google.com/scholar?start=0&q=allintitle:+amphibians+richness+OR+diversity&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1

webpage <- getURL(url)
html_str <- paste(webpage, collapse="\n")

# Find html place holders (2 alternatives!) for number of results,
# and pull the number.
# (!) Strangely Google Scholar gives different numbers of results
# dependent on start value.. i.e., a change from 900 to 980 results
# when changing start = 0 to start = 800

match_no.res <- str_match(html_str, "Results <b>1</b> - <b>(.*?)</b> of <b>(.*?)</b>")
no.res <- match_no.res[1, max(dim(match_no.res))]

if(nchar(no.res) == 0 | is.na(no.res) | nchar(gsub("\\d", "", no.res))) {
match_no.res <- str_match(html_str, "Results <b>1</b> - <b>(.*?)</b> of about <b>(.*?)</b>")
no.res <- match_no.res[1, max(dim(match_no.res))]
}

# Remove punctuation (Google uses decimal commas):
no.res <- as.integer(gsub("[[:punct:]]", "", no.res))

# If there are no results, stop and throw an error message:
if(nchar(no.res) == 0 | is.na(no.res) | nchar(gsub("\\d", "", no.res))) {
stop("\n\n...There is no result for the submitted search string!")}

# Define number of pages with results to be used subsequently
# pages.max = maximum number of pages (chunk with 100 results each)
# to be submitted subsequently.
# Above it was said that no.res varies, depending on start value.
# However, we use ceiling and the change will very unlikely be greater
# than 100, so we may also add one page plus, to be save:
pages.max <- ceiling(no.res/100)+1

# "start" as used in url:
start <- c(100*1:pages.max) - 100

# Collect webpages as list:
urls <- paste("http://scholar.google.com/scholar?start=", start,
          "&q=", search.str,
          "&hl=en&lr=lang_en&num=100&as_sdt=1&as_vis=1",
          sep = "")

webpages <- lapply(urls, getURL)

# Paste all content:
html_str <- paste(unlist(webpages), collapse="\n")

# Pull titles between h3 tags:
match_h3 <- str_match_all(html_str, "<h3>(.*?)</h3>")
match_h3 <- match_h3[[1]][, 2]

# Clean up br-tags:
match_h3 <- gsub("<b>", "", match_h3)
match_h3 <- gsub("</b>", "", match_h3)

# Get id's for different entry types, which have different html-schemes and
# hence will have to be treated differently when cleaning up:
id_books <- grep("BOOK", match_h3)
id_pdfs <- grep("PDF", match_h3)

# The rest is articles:
id_articles <- c(1:length(match_h3))[c(-id_books, -id_pdfs)]

# Check correctness of ids:
# should be as many as number of titles
# sort(c(id_books, id_pdfs, id_articles)) == 1:length(match_h3)

# Get html code for different types of publications
books_raw <- match_h3[id_books]
articles_raw <- match_h3[id_articles]
pdfs_raw <- match_h3[id_pdfs]

# Clean up & pull titles:
if(length(id_books) > 0){
book <- TRUE
b.title_str <- strsplit(books_raw, ">")
b.titles <- rep(NA, length(b.title_str))
for(i in 1:length(b.title_str)){
    b.titles[i] <- substring(b.title_str[[i]][4],
                        1, nchar(b.title_str[[i]][4])-3)}
} else {
    book <- FALSE
}

if(length(id_articles) > 0){
art <- TRUE
a.title_str <- strsplit(articles_raw, ">")
a.titles <- rep(NA, length(a.title_str))
for(i in 1:length(a.title_str)){
    a.titles[i] <- substring(a.title_str[[i]][2],
                        1, nchar(a.title_str[[i]][2])-3)}
} else {
    art <- FALSE
}

if(length(id_pdfs) > 0){
pdf <- TRUE
pdf.title_str <- strsplit(pdfs_raw, ">")
pdf.titles <- rep(NA, length(pdf.title_str))
for(i in 1:length(pdf.title_str)){
    pdf.titles[i] <- substring(pdf.title_str[[i]][4],
                          1, nchar(pdf.title_str[[i]][4])-3)}
} else {
    pdf <- FALSE
}

# Get links:
match_aref <- str_match_all(match_h3, "<a href=\"(.*?)\"")

links <- rep(NA, length(match_aref))
for(i in 1:length(match_aref)){
    if (length(match_aref[[i]][, 2]) == 0)
    links[i] <- ""
    else (links[i] <- match_aref[[i]][, 2])}

# Dataframe with titles and links:
result <- data.frame(
    ARTICLES = NA, BOOKS = NA,
    PDFs = NA, LINKS = links)

if(art == TRUE){
result[id_articles, "ARTICLES"] <- a.titles
}
if(book == TRUE){
result[id_books, "BOOKS"] <- b.titles
}
if(pdf == TRUE){
result[id_pdfs, "PDFs"] <- pdf.titles
}

# Optionally write table with results to system default folder:
if(write.table){
write.table(result, path.expand("~\\GScholarScraper-Result.CSV"),
     row.names = F, sep = ";")
}

# Make a dataframe with word frequencies and a wordcloud:
# if there are too few results stop and throw an error message:
if(no.res < 5){stop("\n\nThere are less than 5 Results, a word cloud may be useless!")}

corpus <- Corpus(DataframeSource(result[, 1:3]))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, function(x)removeWords(x, stopwords()))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)

# Remove strings with numbers:
d <- d[-grep("[1-9]", d$word), ]

# Remove unwanted rubbish (..to be extended?):
rubbish <- c("htmls", "hellip", "amp", "quot")
d <- d[d$word%in%rubbish == FALSE, ]

# Show only frequencies larger than 5:
print(d[d$freq > 5, ])
cat(paste("\n\nNumber of titles submitted =", length(match_h3)))

# Compare retrieved titles and no. of results pulled from first webpage:
cat(paste("\n\nNumber of results as retrieved from first webpage =", no.res))

cat("\n\nBe aware that sometimes titles in Google Scholar outputs
are truncated - that is why, i.e., some mandatory intitle-search
strings may not be contained in all titles\n")

# Print wordcloud:
wordcloud(d$word, d$freq, random.order = F)

return(d)
}


# Example:
# The below search string will search for titles with the words "amphibians"
# and "richness" or "diversity":

# search.str <- "allintitle:+amphibians+richness+OR+diversity"
# d <- GScholarScraper(search.str, write.table = FALSE)