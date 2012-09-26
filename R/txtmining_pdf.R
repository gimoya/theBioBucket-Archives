# pdftotxt: ftp://ftp.foolabs.com/pub/xpdf/xpdfbin-win-3.03.zip

url <- "http://www.noisyroom.net/blog/RomneySpeech072912.pdf"
dest <- tempfile(fileext = ".pdf")
download.file(url, dest, mode = "wb")

exe <- "C:\\xpdfbin-win-3.03\\bin32\\pdftotext.exe"
system(paste("\"", exe, "\" \"", dest, "\"", sep = ""), wait = F)

filetxt <- sub(".pdf", ".txt", dest)
shell.exec(filetxt); shell.exec(filetxt)    # strangely the first try always throws an error..

#install.packages("tm")
library(tm)
 
#install.packages("wordcloud")
library(wordcloud)

txt <- readLines(filetxt) # don't mind warning..

txt <- tolower(txt)
txt <- removeWords(txt, c("\\f", stopwords()))

corpus <- Corpus(VectorSource(txt))
corpus <- tm_map(corpus, removePunctuation)
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
d <- data.frame(freq = sort(rowSums(m), decreasing = TRUE))

# combine similar words
l <- nrow(d)
wordmat <- matrix(rep(NA, l^2), l, l, dimnames = list(row.names(d), row.names(d)))
for (i in 1:ncol(wordmat)) {
   rid <- agrep(colnames(wordmat)[i], rownames(wordmat), max = 0)
   d$matchid[i] <- paste(rid, collapse = "") 
   }

# print wordcloud:
wordcloud(d$word, d$freq)


file.remove(dir(tempdir(), full.name=T)) # remove files