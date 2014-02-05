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
summary(values(slo))

## set values below 25 to NA, these will be transparent
## classes ->                                         1           2           3           4           5           6                                                                         
slo_final <- reclassify(slo, c(-Inf, 25, NA, 25, 30, 25, 30, 35, 30, 35, 40, 35, 40, 45, 40, 45, 50, 45, 50, 90, 50))
hist(slo_final, breaks = 6)
table(values(slo_final))

colv <- rev(heat.colors(6))
KML(slo_final, file="SLOPE_N46_E11.kml", maxpixel = ncell(slo_final), 
    overwrite = T, blur = 2, col = colv)
shell.exec("SLOPE_N46_E11.kml")

## i uploaded the below legend to imgur for latter use in 
## google earth
png(file = "Legend.png", bg = "white")
plot.new()
title(main=list("Slope-Classes in Degrees:", cex=2.7))
legend("center", c("25-30", "30-35", "35-40", "40-45", "45-50", "50+"),
       pch = 15, cex = 3, col = colv, bty = "n")
dev.off()

cord <- t(matrix(bbox(slo_final)[,1]))
placement_legend <- SpatialPointsDataFrame(cord, data.frame(NA))
icon <- NULL
description <- "<a href='http://i.imgur.com/ROcnDNZ.png' <img src='http://i.imgur.com/f4PyQYD.png'</img></a>"
kmlPoints(placement_legend, kmlfile="Legend.kml", kmlname="Slope-Legend", name="Click 'Slope-Legend' link for legend..", 
          description="", icon=icon, kmldescription=description)

shell.exec("Legend.kml")