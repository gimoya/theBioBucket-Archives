library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(XML)
library(RCurl)


## get filesnames
## assuming the data was downloaded already
setwd("D:/GIS_DataBase/DEM")
filenames <- gsub(".hgt", "", dir(pattern=".hgt"))

## process all files and save to "/sqlite"
## then upload to server
## make directory
# dir.create("D:/GIS_DataBase/DEM/slope/sqlite")
setwd("D:/GIS_DataBase/DEM/slope/sqlite")

for (i in 1:length(filenames)))

{
    
    ## get elevation data
    filename <- filenames[i]

    # not suggested: kml_file <- paste0("SLOPE_", filename, ".kml")
    # when packaging to kmz the default name of the kml-file in the folder 'files'
    # should be 'doc.kml'
    kml_file <- "doc.kml"
    zip_file <- paste0(filename, ".zip")
    
    ## read elevation data
    ## assuming it was downloaded already
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

    kmz_file <- paste0("SLOPE_", filename, ".kmz")
    KML(slo_final, file="doc.kmz", maxpixel = ncell(slo_final), overwrite = T, 
        blur = 2, col = colv,
        zip = paste0('"C:\\Program Files\\7-Zip\\7z.exe" a -tzip ')
        )

    file.rename("doc.kmz", kmz_file)

    sqlite_file <- paste0("SLOPE_", filename, ".sqlitedb")
    
	# get the MAPC2MAPC cmd-line version for windows for free here: http://www.the-thorns.org.uk/mapping/
    cmd <- paste0('"C:/Program Files (x86)/MAPC2MAPCNET/mapc2mapcnet.exe" ', normalizePath(kmz_file), ' -mt 12-12 "', getwd(), '/', sqlite_file, '"')
    system(cmd)
    
    # optionally upload to server
    ftpUpload(sqlite_file, paste0("ftp://gimoya:password@gimoya.bplaced.net/Terrain-Overlays/downloads/", sqlite_file))
    
} 