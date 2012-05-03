# world bank indicators for species - 
# I'll check bird species:
require(WDI)
code <- as.character(WDIsearch("bird")[1,1])
bird_data <- WDI(country="all", indicator=code, start=2010, end=2012)

# remove NAs and select values in the range 50 - 1000:
bird_data_sub <- bird_data[!is.na(bird_data$EN.BIR.THRD.NO)&
                           bird_data$EN.BIR.THRD.NO < 1000&
                           bird_data$EN.BIR.THRD.NO > 50, ]

# change in numbers across years 2010 and 2011:
change.no <- aggregate(EN.BIR.THRD.NO ~ country, diff,
                       data = bird_data_sub)
# plot:
par(mar = c(3, 3, 5, 1))
plot(x = change.no[,2], y = 1:nrow(change.no),
     xlim = c(-12, 12), xlab = "", ylab = "",
     yaxt = "n")
abline(v = 0, lty = 2, col = "grey80")
title(main = "Change in Threatened Bird Species in\nCountries with Rich Avifauna (>50)")
text(y = 1:nrow(change.no), 
     x = -2, adj = 1,
     labels = change.no$country)
segments(x0 = 0, y0 = 1:nrow(change.no),
         x1 = change.no[, 2], y1 =  1:nrow(change.no))

# test hypothesis that probability of species decrease is
# equal to probability of increase:
binom.test(sum(change.no < 0), sum(change.no != 0))