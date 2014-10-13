library(RGoogleDocs)
library(tools)
library(RCurl)

install.packages("RGoogleDocs", repos = "http://www.omegahat.org/R", type="source")

gpasswd = "gimoya101"
auth = getGoogleAuth("kay.cichini@gmail.com", gpasswd)
con = getGoogleDocsConnection(auth)

CAINFO = paste(system.file(package="RCurl"), "/CurlSSL/ca-bundle.crt", sep = "")
docs <- getDocs(con, cainfo = CAINFO)

# get file references
hrefs <- lapply(docs, function(x) return(x@access["href"]))
keys <- sub(".*/full/.*%3A(.*)", "\\1", hrefs)
types <- sub(".*/full/(.*)%3A.*", "\\1", hrefs)

# make urls (for url-scheme see: http://techathlon.com/download-shared-files-google-drive/)
# put format parameter for other output formats!
pdf_urls <- paste0("https://docs.google.com/uc?export=download&id=", keys)
doc_urls <- paste0("https://docs.google.com/document/d/", keys, "/export?format=", "txt")


# download documents with your browser
gdoc_ids <- grep("document", types)
lapply(gdoc_ids, function(x) shell.exec(doc_urls[x]))

pdf_ids <- grep("pdf", types, ignore.case = T)
lapply(pdf_ids, function(x) shell.exec(pdf_urls[x]))

