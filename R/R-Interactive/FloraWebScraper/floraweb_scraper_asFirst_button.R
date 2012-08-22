.First <- function() {
require(base)
require(utils)
require(tcltk)

# Purpose: Scrape Floraweb.de for plant species data (photograph, sociology, ecology, anatomy)
# Author: Kay Cichini Date: 2012-06-10
# Output: PDF to folder .~/FLORAWEB
# Packages: XML, RCurl, jpeg
# Licence: cc by-nc-sa

floraweb_scraper <- function(input) {

	# Helper functions:
	# automated package installation:    
	instant_pkgs <- function(pkgs) { 
		pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
		if (length(pkgs_miss) > 0) {
			install.packages(pkgs_miss)
		}
		
		if (length(pkgs_miss) == 0) {
		message("\n ...All packages were already installed!")
		}
		
		# install packages not already loaded:
		pkgs_miss <- pkgs[which(!pkgs %in% installed.packages()[, 1])]
		if (length(pkgs_miss) > 0) {
			install.packages(pkgs_miss)
		}
		
		# load packages not already loaded:
		attached <- search()
		attached_pkgs <- attached[grepl("package", attached)]
		need_to_attach <- pkgs[which(!pkgs %in% gsub("package:", "", attached_pkgs))]
		
		if (length(need_to_attach) > 0) {
		  for (i in 1:length(need_to_attach)) require(need_to_attach[i], character.only = TRUE)
		}
		
		if (length(need_to_attach) == 0) {
		message("\n ...All packages were already loaded!\n")
		}
	}

	pkgs <- c("XML", "RCurl", "jpeg", "gregmisc")
	instant_pkgs(pkgs)

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
		x <- gsub("Â±", "~", x)
		x <- gsub("Â ", "", x)  # pattern for backspaces
	}

	main_function <- function(x) {
		
		# prepare input and get parsed script:
		input1 <- gsub("[[:space:]]", "+", x)
		URL <- paste("http://www.floraweb.de/pflanzenarten/taxoquery.xsql?taxname=", 
			input1, sep = "")
		doc <- htmlParse(URL)
		
		# get returned species names (dismiss last row with additional info):
		sp <- xpathSApply(doc, "//div[@id='contentblock']//a", xmlValue)
		len <- length(sp) - 1
		sp <- sp[1:len]
		
		# get species ids from contentblock:
		con <- getNodeSet(doc, "//div[@id='contentblock']//a")[1:len]
		urls <- sapply(con, xmlGetAttr, "href")
		id_1 <- gsub("[^0-9]", "", urls)
		
		# check matching and assign to resulting dataframe:
		match <- numeric()
		for (i in 1:len) {
			match[i] <- sum(unlist(strsplit(tolower(sp), " ")[i]) %in% unlist(strsplit(x, 
				" ")) == 0)
		}
		df <- data.frame(sp, id_1, match)
		
		# select the one with best match:
		sel <- id_1[rank(df$match)][1]
		
		# build urls for retrieving species data
		url <- paste("http://www.floraweb.de/pflanzenarten/druck.xsql?suchnr=", sel, 
			sep = "")
		
		doc <- htmlParse(url)
		img_src <- xpathSApply(doc, "//*/p[@class=\"centeredcontent\"]/img/@src")
		img_url <- gsub("../", "http://www.floraweb.de/", img_src, fixed = T)
		
		# get infos:
		infos <- xpathSApply(doc, "//div[@id='content']//p", xmlValue)[c(2, 7, 22, 33, 14)]
		
		# replace special characters:
		infos <- spch_sub(infos)
		
		# clean strings:
		infos_1 <- gsub(":\\s*", ": ", infos)
		spl <- strsplit(infos_1, ":")
		
		infos_2 <- 
		unlist(lapply(spl, function(x) {paste(x[1], ifelse(nchar(x[2]) > 1, trim(x[2]), "keine  Angaben"), sep = ": ")
						}
				)
			)
		
		# make dir to save data:
		dir.create(path.expand("~/FLORAWEB/"), showWarnings = F)
		setwd(path.expand("~/FLORAWEB/"))
		
		# download image:
		download.file(img_url, "floraweb.jpg", mode = "wb")
		
		# open device:
		pdf(paste(input1, "FloraWeb.pdf", sep = "+"), paper = "a4")
		
		# read image:
		img <- readJPEG("floraweb.jpg")
		w <- dim(img)[2]
		h <- dim(img)[1]
		
		# print img to plot region:
		par(mar = rep(0, 4), oma = rep(0, 4), mfrow = c(2, 1))
		plot(NA, xlim = c(0, w), ylim = c(0, h), xlab = "", ylab = "", axes = F, type = "n", 
			yaxs = "i", xaxs = "i", asp = 1)
		rasterImage(img, 0, 0, w, h)
		
		# print text:
		plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "", ylab = "", axes = F, type = "n", 
			yaxs = "i", xaxs = "i")
		# text left intendent and center adjustment:
		l <- 0.5
		c_adj <- c(0.5, 0.5)
		
		# plot text:
		text(l, 0.9, paste("Eingabe = ", x, " / Gefunden = ", infos_2[1], sep = ""), font = 2, 
			adj = c_adj, cex = 0.7)
		text(l, 0.5, paste(strwrap(infos_2[-1], width = 112), collapse = "\n"), adj = c_adj, 
			cex = 0.7)
		
		# Credit:
		text(l, 0.05, "Die hier verwendeten Daten sind der Internet-Seite FloraWeb.de entnommen.", 
			adj = c_adj, cex = 0.4, font = 3)
		
		graphics.off()
		message(paste(infos[1], "PDF wurde erzeugt\n\n", sep = "\n -- "))
		
		# remove jpegs:
		unlink(dir(pattern = ".jpg"))
	}

	file <- dir("C:/Users", pattern = "PFLLISTE.csv$", recursive = T, full.names = T)
	input <- scan(file, what = character(), sep = ";")
	message("\nVerwendete Pflanzen Namen:\n")
      print(as.data.frame(input))

    # Finally, using the main function:
    invisible(lapply(input, main_function))
}


PressedOK <- function()
{   
    floraweb_scraper()
    invisible(lapply(dir(pattern = ".pdf"), FUN = shell.exec))
    tkmessageBox(message="PDFs wurden erzeugt!")
    q(save="no")
    tkdestroy(tt)
}

tt <- tktoplevel()
tktitle(tt) <- "Floraweb Scraper"
OK.but <- tkbutton(tt, text = "     OK      ", command = PressedOK)
quit <- tkbutton(tt, text = "Abbrechen", 
          command = function() {
            q(save = "no")
            tkdestroy(tt)
            })
tkgrid(OK.but, quit,
tklabel(tt, 
text="  Speichere deine Pflanzenliste 
  mit dem Namen PFLLISTE.csv
  irgendwo unter C:/Users/ und 
  drücke dann OK!"))

tkfocus(tt)

}

save.image("C:/Users/Kay/Dropbox/R-Programs/FloraWebScraper/.RData")