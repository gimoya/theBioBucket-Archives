## Extract ways from OpenStreetMaps Export
## and save to file

library(rgdal)
library/maptools)
library(osmar)

path <- "C:/Users/Kay/Downloads/"

# file export from OpenStreetMaps:
file <- "map.osm"
src <- osmsource_osmosis(file = sprintf("%s%s", path, file))

raw <- readLines(sprintf("%s%s", path, file))
munde <- as_osmar(xmlParse(raw))
summary(munde)

tr_ids <- find(munde, way(tags(k=="highway")))
tr_ids <- find_down(munde, way(tr_ids))
trks <- subset(munde, ids = tr_ids)

str(trks)

plot(munde)
plot_ways(trks, add = TRUE, col = "green")

# convert to spatial object (SpatialLinesDataFrame)
# and save to whatever format you like..
tr_line <- as_sp(trks, "lines")
writeOGR(tr_line, dsn="OSM_R_extract.sqlite", layer="osm_tracks", driver="SQLite")