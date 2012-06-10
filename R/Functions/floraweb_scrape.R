# Purpose: Collect plant species data (photography, ecology, anatomy)
# Author: Kay Cichini
# Date: 2012-06-10
# Output: PDF
# Packages: XML, RCurl, jpeg, 
# Licence: CC by-nc-sa



floraweb_scraper <- function(x) {
    
    # I didn't get around this encoding issue other than with gsub..
    spch_sub <- function(x) {
        x <- gsub("Ã¼", "ü", x)
        x <- gsub("Ã¤", "ä", x)
        x <- gsub("Ã¶", "ö", x)
        x <- gsub("Ã„", "Ä", x)
        x <- gsub("Ãoe", "Ü", x)
        x <- gsub("Ã¼", "Ä", x)
        x <- gsub("Ã–", "Ö", x)
        x <- gsub("ÃŸ", "ß", x)
        x <- gsub("Ã©", "é", x)
        x <- gsub("Ã-", "í", x)
        x <- gsub("Ã¡", "á", x)
        x <- gsub("Â ", "", x)  # pattern for backspaces
    }
    
    # automated package installation:
    pkgs <- c("RCurl", "XML", "jpeg")
    
    pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
    if (length(pkgs_miss) > 0) {
        install.packages(pkgs_miss)
    }
    
    # load packages:
    require(XML)
    require(RCurl)
    require(jpeg)
    
    # get parsed script:
    input <- x
    input1 <- gsub("[[:space:]]", "+", input)
    script <- getURL(paste("http://www.floraweb.de/pflanzenarten/taxoquery.xsql?taxname=", input1, "&Button=Suche+starten&max-rows=10&skip-rows=0", sep = ""))
    doc <- htmlParse(script)
    
    # length of no_1:
    len <- length(getNodeSet(doc, "//div[@id='contentblock']//a")) - 1
    
    # get contentblock:
    con <- getNodeSet(doc, "//div[@id='contentblock']//a")[1:len]
    
    # get returned species names:
    sp <- xpathSApply(doc, "//div[@id='contentblock']//a", xmlValue)[1:len]
    
    # get species ids:
    urls <- sapply(con, xmlGetAttr, "href")
    no_1 <- gsub("[^0-9]", "", urls)
    
    # check matching and assign to resulting dataframe:
    match <- numeric()
    for (i in 1:len) {
        match[i] <- sum(unlist(strsplit(tolower(sp), " ")[i]) %in% unlist(strsplit(input, " ")) == 0)
    }
    df <- data.frame(sp, no_1, match)
    
    # select the one with best match:
    sel <- no_1[rank(df$match)][1]
    
    # build urls for retrieving species data
    url <- paste("http://www.floraweb.de/pflanzenarten/foto.xsql?suchnr=", sel, sep = "")
    
    script <- getURL(url)
    doc <- htmlParse(script)
    img_src <- xpathSApply(doc, "//img[@class='image_float_left']", xmlAttrs)["src", ]
    no_2 <- gsub("[^0-9]", "", img_src)
    
    img_url <- paste("http://www.floraweb.de/bilder/Arten/", no_2, ".jpg", sep = "")
    
    # make dir to save image:
    dir.create(path.expand("~/FLORAWEB/"), showWarnings = F)
    setwd(path.expand("~/FLORAWEB/"))
    download.file(img_url, paste(no_2, ".jpg", sep = ""), mode = "wb")
    
    # get infos:
    sp_name <- xpathSApply(doc, "//div[@id='content']//p", xmlValue)[2]
    best_hilfe <- xpathSApply(doc, "//div[@id='content']//p", xmlValue)[4]
    
    # get url and page for ecology info:
    oek_url <- paste("http://www.floraweb.de/pflanzenarten/", xpathSApply(doc, "//ul[@class='nav']//a", xmlAttrs)[5], sep = "")
    script <- getURL(oek_url)
    doc <- htmlParse(script)
    hab_form <- xpathSApply(doc, "//div[@id='content']//p", xmlValue)[3]
    hab_soz <- xpathSApply(doc, "//div[@id='content']//p", xmlValue)[5]
    
    # replace special characters:
    sp_name <- spch_sub(sp_name)
    best_hilfe <- spch_sub(best_hilfe)
    hab_form <- spch_sub(hab_form)
    hab_soz <- spch_sub(hab_soz)
    
    # open device:
    pdf(paste(spch_sub(df$sp[df$no_1 == sel]), "_FloraWeb.pdf", sep = ""), paper = "a4")
    
    # read image:
    img <- readJPEG(paste(no_2, ".jpg", sep = ""))
    w <- dim(img)[2]
    h <- dim(img)[1]
    
    # print img to plot region:
    par(mar = rep(0, 4), oma = rep(0, 4), mfrow = c(2, 1))
    plot(NA, xlim = c(0, w), ylim = c(0, h), xlab = "", ylab = "", axes = F, type = "n", yaxs = "i", xaxs = "i", asp = 1)
    rasterImage(img, 0, 0, w, h)
    
    plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = F, type = "n", yaxs = "i", xaxs = "i")
    
    # text left intendent and center adjustment:
    l <- 0.5
    c_adj <- c(0.5, 0.5)
    # plot text:
    text(l, 0.92, "- Such-Eingabe und Ergebnis -", font = 2, adj = c_adj, cex = 0.8)
    text(l, 0.86, paste(input, sp_name, sep = " / "), font = 2, adj = c_adj, cex = 0.7)
    
    # Formation:
    text(l, 0.74, paste(strwrap(hab_form, width = 112), collapse = "\n"), adj = c_adj, cex = 0.7)
    
    # Soziologie:
    text(l, 0.61, paste(strwrap(hab_soz, width = 112), collapse = "\n"), adj = c_adj, cex = 0.7)
    
    # Bestimmungshilfe:
    text(l, 0.4, "- Bestimmungshilfe -", font = 2, adj = c_adj, cex = 0.8)
    text(l, 0.34, paste(strwrap(best_hilfe, width = 112), collapse = "\n"), adj = c(0.5, 1), cex = 0.7)
    
    # Credit:
    text(l, 0.1, "Die hier verwendeten Daten sind der Internet-Seite FloraWeb.de entnommen.", adj = c_adj, cex = 0.4, font = 3)
    
    graphics.off()
    message(paste(sp_name, "PDF created\n\n", sep = "\n -- "))
    
    # remove jpegs:
    unlink(dir(pattern = ".jpg"))
}

# Examples:
pfl_liste <- c("tripl per", "ophrys ins")
lapply(pfl_liste, FUN = floraweb_scraper)
lapply(dir(pattern = ".pdf"), FUN = shell.exec)