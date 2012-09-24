library(dismo)
 
### get GBIF data with function (if sp = T, a spatial object is returned):
sp <- c("campanula", "cochlearifolia") 
distr <- gbif(sp[1], sp[2])

### set spatial coordinates
coordinates(distr) <- c("lon", "lat")

### Subsetting
table(distr@data$country)                        # see occurrences by country
distr.at <- subset(distr, distr$country=="AT")   # select only distr in Austria

### check on a simple plot
plot(distr.at, pch=20, cex=2, col="steelblue")
plot(wrld_simpl, add=T)
summary(distr.at)

# you could also use function gmap in "dismo"
points.at <- as.data.frame(distr.at)
points.at$x <- distr.at@coords[,"lon"]
points.at$y <- distr.at@coords[,"lat"]
at.map <- gmap(points.at, type="roadmap")

distr.at.merc <- Mercator(points.at[,c("x","y")])  
                                      # Google Maps are in Mercator projection.
                                      # This function projects the points to that
                                      # projection to enable mapping
plot(at.map)
points(distr.at.merc, pch=20, col="red")
 
### Plotting onto a Google Map using googleVis
library(googleVis)
points.at <- as.data.frame(distr.at)
points.at$latlon <- paste(distr.at@coords[,"lat"], distr.at@coords[,"lon"], sep=":")
map.at <- gvisMap(points.at, locationvar="latlon", tipvar="locality",
                  options = list(showTip=T, showLine=F, enableScrollWheel=TRUE,
                           useMapTypeControl=T, width=700, height=500))
plot(map.at)
print(map.at)    # HTML suitable for a web page

### Plotting onto a Google Map using googleVis
library(googleVis)
points <- as.data.frame(distr)
points$latlon <- paste(distr@coords[,"lat"], distr@coords[,"lon"], sep=":")
map <- gvisMap(points, locationvar="latlon", tipvar="locality",
               options = list(showTip=T, showLine=F, enableScrollWheel=TRUE,
                              useMapTypeControl=T, width=600, height=500, 
                              maptype="normal"))
plot(map)
print(map)