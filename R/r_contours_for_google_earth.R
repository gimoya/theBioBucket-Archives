library(rgdal)
library(raster)
library(maptools)
library(rgeos)
library(RCurl)

# dem in hgt-format downloaded from http://www.viewfinderpanoramas.org/dem3.html#alps
# as done here: 
# http://thebiobucket.blogspot.co.at/2013/06/use-r-to-bulk-download-digital.html
# splitted into tiles for easier handling and saved to tiff as done here:
# http://thebiobucket.blogspot.co.at/2014/03/use-gdal-from-r-console-to-split-raster.html

setwd("D:/GIS_DataBase/DEM")
(filenames <- gsub(".tif", "", dir(pattern = ".tif")))

## make folder for output and set directory
# dir.create('D:/GIS_DataBase/DEM/contours')
setwd("D:/GIS_DataBase/DEM/contours")

## funtion make_kml_contours
## arguments
## step: altitude inbetween contours, starting at 0 m
## simplify: 1-0, 1 is no generalization, 0 is straight line
## ftp: optional ftp uload

make_kml_contours <- function(filename, step = 100, simplify = 0.001, ftp = F) 

{
    ## coerce into SpatialGridDataFrame
    dem <- readGDAL(paste0("D:/Gis_Database/DEM/", filename, ".tif"))
    
    ## make image object for contourLines function
    im <- as.image.SpatialGridDataFrame(dem)
    # check: summary(im$z)
    cl <- contourLines(im, levels = seq(0, max(im$z), step))
    
    ## back convert to SpatialLinesDataFrame
    SLDF <- ContourLines2SLDF(cl)
    proj4string(SLDF) <- CRS("+proj=longlat +datum=WGS84")
    
    ## simplify
    simplSLDF <- gSimplify(SLDF, tol = simplify)
    
    ## view results
    # image(dem, col = gray.colors(20))
    # plot(simplSLDF, add = T)
    
    ## convert simplified SLDF to KML (btw, that's how to extract IDs unlist(lapply(slot(simplSLDF, 'lines'), function(x) slot(x, 'ID'))) )   
    out <- sapply(slot(simplSLDF, "lines"), function(x) {
        # get meter level, by picking from sequence by ID: ID = 1 -> 1*step m, ID = 2, 2*step m, etc.
        m <- seq(0, max(im$z), step)[as.numeric(gsub("C_", "", slot(x, "ID")))]
        # make thicker lines at 100 and 500 m Isolines
        kmlLine(x, name = m, description = paste0(m, "m-Isoline"), col = "#FCCD47", lwd = ifelse(m%%100 == 0, ifelse(m%%500, 2, 1.25), 0.75))
    })
    
    # write KML
    tf <- tempfile()
    kmlFile <- file(tf, "w")
    cat(kmlLine(kmlname = "Contour Lines", kmldescription = "<i>Contour lines by Kay Cichini, see <a href=\"htp://gimoya.bplaced.net/terrain-overlays.blogspot.co.at\">Terrain-Overlays</a> for details</i>")$header, 
        file = kmlFile, sep = "\n")
    cat(unlist(out["style", ]), file = kmlFile, sep = "\n")
    cat(unlist(out["content", ]), file = kmlFile, sep = "\n")
    cat(kmlLine()$footer, file = kmlFile, sep = "\n")
    close(kmlFile)
    
    kmlName <- paste0("CONTOURS_", filename, ".kml")
    file.rename(tf, kmlName)
    if (ftp == T) ftpUpload(kmlName, paste0('ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/', kmlName))
}

for (filename in filenames[2:136]) 
      {
  	tryCatch(make_kml_contours(filename, step = 25, simplify = 0.0001, ftp = T), 
               error = function(e) message(paste0("\n..something happend with dataset ", filename, ":\n", e)))
      cat("File ", filename, " done!..\n")
	}

