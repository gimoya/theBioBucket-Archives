library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(XML)
library(RCurl)
library(R2HTML)

## make directory
dir.create("D:/GIS_DataBase/DEM/")
setwd("D:/GIS_DataBase/DEM/")

## get files and names
doc <- htmlParse("http://www.viewfinderpanoramas.org/dem3.html#alps")
urls <- paste0("http://www.viewfinderpanoramas.org", xpathSApply(doc, "//*/a[contains(@href,\"/dem1/N4\")]/@href"))
zip_files <- gsub(".*dem1/(\\w+\\.zip)", "\\1", urls)
filenames <- gsub(".zip", "", zip_files)

## process all files
for (i in 1:length(filenames)) 

{
    
    ## get elevation data
    filename <- filenames[i]

    # not suggested: kml_file <- paste0("SLOPE_", filename, ".kml")
    # when packaging to kmz the default name of the kml-file in the folder 'files'
    # should be 'doc.kml'
    kml_file <- "doc.kml"
    zip_file <- paste0(filename, ".zip")

    if(!file.exists(paste0(filename,".hgt")))
    {
       url <- paste0("http://www.viewfinderpanoramas.org/dem1/", zip_file)
       download.file(url, zip_file)
       unzip(zip_file)
       unlink(zip_file)
    }   
    
    ## read elevation data
    x <- readGDAL(paste0("D:/Gis_Database/DEM/", filename, ".hgt"))
    
    ## coerce into RasterLayer object as desired by terrain function
    y <- raster(x)
    
    ## calculate terrain
    slo <- terrain(y, opt = "slope", unit = "degrees", df = F)
    
    ## check summary(values(slo))
    
    ## set values below 25 to NA, these will be transparent classes -> 1 2 3 4 5 6
    slo_final <- reclassify(slo, c(-Inf, 25, NA, 25, 30, 25, 30, 35, 30, 35, 40, 35, 40, 45, 40, 45, 50, 45, 50, 90, 50))
    
    ## inspect data hist(slo_final, breaks = 6) table(values(slo_final))
    
    ## set colors for slope angle classes and save as kml
    colv <- rev(heat.colors(6))
    KML(slo_final, file = kml_file, maxpixel = ncell(slo_final), overwrite = T, blur = 2, col = colv)
    
    ## add the default top node with full namespace info
    x <- readLines(kml_file, warn = F)
    x[2] <- '<kml xmlns="http://www.opengis.net/kml/2.2" xmlns:gx="http://www.google.com/kml/ext/2.2" xmlns:kml="http://www.opengis.net/kml/2.2" xmlns:atom="http://www.w3.org/2005/Atom">'
    writeLines(x, kml_file)

    ## add transparency to kml. the namespace issue (kml:) is explained in the getNodeSet(XML) R documentation under Details
    doc <- xmlInternalTreeParse(kml_file)
    over_node <- getNodeSet(doc, "/kml:kml/kml:GroundOverlay", c(kml = "http://www.opengis.net/kml/2.2"))
    color_node <- newXMLNode("color", attr = "6bffffff")
    over_node[[1]] <- addChildren(over_node[[1]], color_node, at=1)
    
    ## save kml back & zip to kmz you will need to put in a suitable zipping program (i don't have 7-zip on the PATH, so I need to use an explicit system call..
    saveXML(doc, kml_file)
    dir.create("files")
    file.copy("doc.png", "files")
    
    kmz_file <- paste0("SLOPE_", filename, ".kmz")
    cmd <- paste0("\"C:\\Program Files\\7-Zip\\7z.exe\"", " a -tzip ", kmz_file, " ", kml_file, " files")
    cat(cmd)
    system(cmd)
    unlink("files", recursive = T, force = T)
    file.remove(c("doc.png", kml_file))
    # shell.exec(paste0('SLOPE_', filename, '.kmz'))
    
    # optionally upload to server
    ftpUpload(kmz_file, paste0("ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/", kmz_file))
    
} 

## i externally uploaded the below legend to my server for latter use in google earth
png(file = "Legend.png", bg = "white")
plot.new()
title(main = list("Slope Angle Classes", cex = 2.7))
legend("center", c("25°-30°", "30°-35°", "35°-40°", "40°-45°", "45°-50°", "50°+"), pch = 15, cex = 3, col = colv, bty = "n")
dev.off()

cord <- t(matrix(bbox(slo_final)[, 1]))
placement_legend <- SpatialPointsDataFrame(cord, data.frame(NA))
icon <- NULL
description <- "<img src='http://gimoya.bplaced.net/Terrain-Overlays/Legend.png'></img>"
kmlPoints(placement_legend, kmlfile = "Legend.kml", kmlname = "Slope-Legend", name = "Click 'Slope-Legend' link for legend..", description = "", icon = icon, kmldescription = description)

# shell.exec('Legend.kml')

## Now, produce HTML-table with file-list and hyperlinks:

kmz_files <- paste0("SLOPE_", sort(filenames), ".kmz")
sqlitedb_files <- paste0("SLOPE_", sort(filenames), ".sqlitedb")

c1 <- "Slope Angle for Alps-region in KMZ-format"
c2 <- "... in SQLITED-format"
df <- data.frame(a = paste0("<a href='http://gimoya.bplaced.net/Terrain-Overlays/download-area/", kmz_files, "'>",kmz_files, "</a>"),
                 b = paste0("<a href='http://gimoya.bplaced.net/Terrain-Overlays/download-area/", sqlitedb_files, "'>", sqlitedb_files, "</a>")
                 )

HTML(df, file = "SLOPE_files_table.html", Border = 0, caption = c(c1, c2), captionalign="top", row.names=F)