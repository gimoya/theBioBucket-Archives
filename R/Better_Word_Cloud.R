# File-Name: Better_Wordc_Cloud.R
# Date: 2011-12-07
# Author: Kay Cichini
# Email: kay.cichini@gmail.com
# Purpose: create a word-cloud with spatial meaning
# Packages Used: tm, vegan, stringr, maptools
# Output File: wcloud.pdf
# Licence: CC BY-NC-SA

# Load packages and data
library(tm)
library(stringr)
library(vegan)
library(maptools)

# I SAID:
ISAID <- c("You are so beautiful. You are better than the rest. You are tall and you are so very strong and no one can do you harm. You will surely stand and will not fall until you hear the last call.")

# YOU SAID:
YOUSAID <- c("I am the very best. In fact I am better than the rest. I am tall and I am strong and no one can do me harm. I will stand and will not fall until I hear the last call.")

#########################################################################################

# preparing data ISAID:
# keep alphanumeric signs and fullstop, remove anything else (all other punctuation)
ISAID <- gsub("[^[:alpha:][:space:].]", "", ISAID)

# seperate sentences:
ISAID.sent <- as.data.frame(strsplit(ISAID, "\\."))
ISAID.corp <- Corpus(DataframeSource(ISAID.sent))
inspect(ISAID.corp)

cntrl <- list(stopwords = F, minWordLength = 1)

# Create a Term-Document matrix
tdm <- TermDocumentMatrix(ISAID.corp, control = cntrl)
m.ISAID <- as.matrix(tdm)
v.ISAID <- sort(rowSums(m.ISAID), decreasing = TRUE)
d.ISAID <- data.frame(word = names(v.ISAID), freq = v.ISAID)

#########################################################################################

# preparing data YOUSAID:

# keep alphanumeric signs and fullstop, remove anything else (all other punctuation)
YOUSAID <- gsub("[^[:alpha:][:space:].]", "", YOUSAID)

# seperate sentences:
YOUSAID.sent <- as.data.frame(strsplit(YOUSAID, "\\."))
YOUSAID.corp <- Corpus(DataframeSource(YOUSAID.sent))
inspect(YOUSAID.corp)

cntrl <- list(stopwords = F, minWordLength = 1)

# Create a Term-Document matrix
tdm <- TermDocumentMatrix(YOUSAID.corp, control = cntrl)
m.YOUSAID <- as.matrix(tdm)
v.YOUSAID <- sort(rowSums(m.YOUSAID), decreasing = TRUE)
d.YOUSAID <- data.frame(word = names(v.YOUSAID), freq = v.YOUSAID)

#########################################################################################

# transpose matrix for use in subsequent NMDS-ordination:
m1.ISAID <- t(m.ISAID); m1.YOUSAID <- t(m.YOUSAID)

# Combining speaker's data - NMDS-Ordination:
speaker = factor(append(rep("ISAID", nrow(m1.ISAID)), rep("YOUSAID", nrow(m1.YOUSAID))))
index = c(paste("I", rownames(m1.ISAID)), paste("YOU", rownames(m1.YOUSAID)))
words = unique(append(colnames(m1.ISAID), colnames(m1.YOUSAID)))
dimnames = list(index, words)
m.speakers <- matrix(nrow = length(speaker),
                  ncol = length(words),
                  data = 0,
                  dimnames = dimnames)

m.speakers[paste("I", rownames(m1.ISAID)),
          colnames(m1.ISAID)] <- m1.ISAID
m.speakers[paste("YOU", rownames(m1.YOUSAID)),
          colnames(m1.YOUSAID)] <- m1.YOUSAID

# see what words are shared which are used uniquely by one of the two:
aggregate(m.speakers, by = list(speaker), FUN = sum)

#########################################################################################

# NMDS-Ordination:
sol.speakers <- metaMDS(m.speakers, dist = "euclidean", k = 2,
                      trymax = 10, autotransform = F, noshare = 0)

# Ordination plot:
setwd(tempdir())

pdf("wcloud.pdf", height = 5, width = 5)
par(mar = c(0, 0, 0, 0), oma = rep(0, 4))

  ordiplot(sol.speakers , type = "n")
  ordispider(sol.speakers, group = speaker, col = "grey50")

  # this is for representation of word frequencies:
  cex.lab <- (colSums(m.speakers)/sum(colSums(m.speakers))*length(colSums(m.speakers)))^0.85
  pch.pts <- substring(speaker, 1, 1)
  points(sol.speakers, pch = pch.pts, cex = 0.85, col = 2, font = 2)

  # using pointLabel() from maptools package should avoid overplotting of words.
  # however, i also add some jitter because i though saw some overplotting
  sol.speakers$species <- jitter(sol.speakers$species, amount = 0.2)
  x = as.vector(sol.speakers$species[,1])
  y = as.vector(sol.speakers$species[,2])
  w = row.names(sol.speakers$species)
  col.lab = rgb(0.2, 0.5, 0.4, alpha = 0.6)
  pointLabel(x, y, w, cex = cex.lab, col = col.lab)

graphics.off()

# open pdf:
browseURL(paste(tempdir(), "/wcloud.pdf", sep = ""))