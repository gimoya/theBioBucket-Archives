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

## function make_kml_contours
## arguments
## intv: altitude level for contours, starting at 0 m
## simplify: 1-0, 0 is no generalization, 1 is straight line
## ftp: optional ftp uload

make_kml_contours <- function(filename, intv = 100, simplify = 0.001, ftp = F) 

{
    ## coerce into SpatialGridDataFrame
    dem <- readGDAL(paste0("D:/Gis_Database/DEM/", filename, ".tif"))
    
    ## make image object for contourLines function
    im <- as.image.SpatialGridDataFrame(dem)

    # check validity of rasters
    if ( sum(is.na(im$z)) > 0 )
         return(cat("\nNAs contained in altitude data: ..exiting function!\n"))
    if ( max(im$z)-min(im$z) < intv) 
         return(cat("\nAltitude gradient is smaller than contour lines' interval: ..exiting function!\n"))

    # levels: if lower than sealevel, then take ceiling, otherwise floor of min/max
    # by division/multiplying by intervall, we fit values to nearest 'pretty' intervall
    fr <- ifelse ( min(im$z) < 0, ceiling(min(im$z)/intv)*intv, floor(min(im$z)/intv)*intv )
    to <- ifelse ( max(im$z) < 0, ceiling(max(im$z)/intv)*intv, floor(max(im$z)/intv)*intv )

    cl <- contourLines(im, levels = seq(fr, to, intv))
   
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
        m <- unique(sapply(cl, function(x) x$level))[as.numeric(gsub("C_", "", slot(x, "ID")))]
        # make thicker lines at 250 and 500 m Isolines
        kmlLine(x, name = paste0(m, "m-Isoline"), description="-", col = "#FCCD47", lwd = ifelse(m%%250 == 0, ifelse(m%%500 == 0, 1.8, 1.3), 0.5))
    })
    
    # write KML
    tf <- tempfile()
    kmlFile <- file(tf, "w")
    cat(kmlLine(kmlname = "Contour Lines", kmldescription = "<i>50 m Isolines by Kay Cichini, see <a href=\"htp://gimoya.bplaced.net/terrain-overlays.blogspot.co.at\">Terrain-Overlays</a> for details</i>")$header, 
        file = kmlFile, sep = "\n")
    cat(unlist(out["style", ]), file = kmlFile, sep = "\n")
    cat(unlist(out["content", ]), file = kmlFile, sep = "\n")
    cat(kmlLine()$footer, file = kmlFile, sep = "\n")
    close(kmlFile)
    
    kmlName <- paste0("CONTOURS_", filename, ".kml")
    file.rename(tf, kmlName)
    if (ftp == T) ftpUpload(kmlName, paste0('ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/', kmlName))
}

for (filename in filenames[3:length(filenames)]) 
      {
  	tryCatch(make_kml_contours(filename, intv = 50, simplify = 0.00005, ftp = F), 
               error = function(e) message(paste0("\n..something happend with dataset ", filename, ":\n", e)))
      cat("File ", filename, " done!..\n")
	}


# check for errors!
l <- readClipboard(****log****)
h <- l[grep("happend with", l)]
i <- 0
for (f in filenames) { 
   i=i+1
   if ( sum(grepl(f, h)) )  print(c(i, f))
}

