library(rgeos)
library(rgdal)
library(raster)
library(maptools)
library(sp)


## make directory
dir.create("D:/GIS_DataBase/DEM/")
setwd("D:/GIS_DataBase/DEM/")
 
## get elevation data
url <- "http://www.viewfinderpanoramas.org/dem1/N46E011.zip" 
download.file(url, "N46E011.zip") 
unzip("N46E011.zip")
unlink("N46E011.zip")

## read elevation data
x <- readGDAL("D:/Gis_Database/DEM/N46E011.hgt")

## coerce into RasterLayer object as desired by terrain function
y <- raster(x)

## calculate terrain
slo <- terrain(y, opt = "slope", unit = "degrees", df = F)

## set values below 25 to NA, these will be transparent
## classes ->                                              1           2           3           4           5          6                                                                         
slo_final <- reclassify(slo, c(-Inf, 25, NA, 25, 30, 25, 30, 35, 30, 35, 40, 35, 40, 45, 40, 45, 50, 45, 50, 90, 50))
hist(slo_final, breaks = 6)
table(values(slo_final))

colv <- rev(heat.colors(6))
KML(slo_final, file="SLOPE_N46_E11.kml", maxpixel = ncell(slo_final), 
    overwrite = T, blur = 2, col = colv)
shell.exec("SLOPE_N46_E11.kml")

## i uploaded the below legend to picasa for latter use in 
## google earth
png(file = "Legend.png", bg = "white")
plot.new()
legend("center", c("+25", "+30", "+35", "+40", "+45", "+50"),
       pch = 15, cex = 3, col = colv, bty = "n")
dev.off()

cord <- t(matrix(bbox(slo_final)[,1]))
placement_legend <- SpatialPointsDataFrame(cord, data.frame(NA))
icon <- "http://maps.google.com/mapfiles/kml/paddle/L.png"
description <- paste0("<a href='https://picasaweb.google.com/lh/photo/4q_GS7JFw5mgFaiUhN2UI5RBqZqMYMGBNmKz6sPyj3M?feat=embedwebsite'><img src='https://lh3.googleusercontent.com/-Enra0zPRGIg/UvFZsg8PqmI/AAAAAAAADy0/ThVoI9QwArQ/s144/Legend.jpg' style="width:auto;'",
                      "</a>")
kmlPoints(placement_legend, kmlfile="Legend.kml", kmlname="Slope-Legend", name="Click me for legend..", 
          description=description, icon=icon, kmldescription="Legend for slope-classes")

shell.exec("Legend.kml")