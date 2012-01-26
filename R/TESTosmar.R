library(osmar)
 
# this pulls the data from the OSM-Api:
mydistrict <- get_osm(relation(85647), full = TRUE)
 
# make a spatial object:
mydistrict_sp <- as_sp(mydistrict, what = 'lines')
summary(mydistrict_sp)
 
# just for fun i'll plot some bubbles on my map:
mydata <- data.frame(x = runif(100, 12.300, 12.550),
                     y = runif(100, 47.300, 47.550),
                     data = sample(1:30, 100, replace = T))
mydata_sp <- SpatialPointsDataFrame(mydata[, c('x', 'y')], data=mydata,
                                    proj4string=CRS('+proj=longlat +datum=WGS84'))
# see what's in there:
summary(mydata_sp)
 
# plot:
setwd(tempdir())
pdf("testOSM.pdf")
par(oma = rep(0, 4))
mycex = (mydata_sp@data[,'data']/max(mydata_sp@data[,'data'])*2)^2
plot(mydistrict_sp, axes = T)
colpts = rgb(0.2, 0.5, 0.4, alpha = 0.6)
points(x = mydata_sp@coords[,'x'],
       y = mydata_sp@coords[,'y'],
       cex = mycex, col = 0,
       pch = 21, bg = colpts)
title('MyData in MyDistrict')

# legend:
l1 = min(mydata_sp@data[,'data'])
l2 = round(mean(mydata_sp@data[,'data']), 0)
l3 = max(mydata_sp@data[,'data'])
l = c(l1, l2, l3)
lcex = (c(l/l3*2))^2
points(x = rep(12, 3), y = seq(47.7, 47.6, -.05),
       cex = lcex, col = 0,
       pch = 21, bg = colpts)
text(l, x = rep(12, 3) +.045, y = seq(47.7, 47.6, -.05))
     
graphics.off()

# open map:
shell.exec("testOSM.pdf")