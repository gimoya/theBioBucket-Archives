library(XML)

## function to exclude every i-th element:
simplify <- function(x, ith) x[-1 * seq(1, length(x), ith)]

simplify_kml_contour_node <- function(node, ith = 5, cut = 10, min_for_simpl = 20) 

## every <ith> vertex will be excluded lines with less than <cut> vertices will be
## removed, segments with less than <min_for_simpl> vertices will not be simplified
## in any case coordinates will be rounded to 5-th decimal place

{
    
    LineString <- xmlValue(node)
    LineStrSplit <- strsplit(unlist(strsplit(LineString, "\\s")), ",")
    x <- round(as.numeric(sapply(LineStrSplit, "[[", 1, simplify = T)), 5)
    y <- round(as.numeric(sapply(LineStrSplit, "[[", 2, simplify = T)), 5)
    z <- round(as.numeric(sapply(LineStrSplit, "[[", 3, simplify = T)), 5)
    
    # remove Lines with less than <cut> vertices, and exit
    if (length(x) < cut) {
        removeNodes(node, free = T)
        stop()
        # else, for Lines longer than <min_for_simpl> vertices, simplify Lines
    } else {
        if (length(x) >= min_for_simpl) {
            x <- simplify(x, ith)
            y <- simplify(y, ith)
            xmlValue(node) <- paste(paste(x, y, z, sep = ","), collapse = " ")
        } else {
            # all other cases, will be just rounded
            xmlValue(node) <- paste(paste(x, y, z, sep = ","), collapse = " ")
        }
    }
}

kml_simplify <- function(kml_file)
	{
	doc <- xmlInternalTreeParse(kml_file)
	nodes <- getNodeSet(doc, "/kml:kml/kml:Document/kml:Folder/kml:Placemark/kml:LineString/kml:coordinates", 
                          c(kml = "http://www.opengis.net/kml/2.2"))
	system.time(lapply(nodes, simplify_kml_contour_node))
	saveXML(doc, file)
	}

## get KML-files and parse them
setwd("D:/GIS_DataBase/DEM/contours")
kml_files <- dir(pattern = ".kml")
kml_file <- kml_files[1]
doc <- xmlInternalTreeParse(kml_file)
	nodes <- getNodeSet(doc, "/kml:kml/kml:Document/kml:Folder/kml:Placemark/kml:LineString/kml:coordinates", 
                    c(kml = "http://www.opengis.net/kml/2.2"))

kml_simplify(kml_file)
lapply(kml_files, kml_simplify)
