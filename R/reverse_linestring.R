## name:          ReverseKmlPath   
## use:           Reverser a KML-path by matching its 
## arguments:     PATH_TO_DOC the path to the KML-file
##                NAME the value of the name tag, function uses partial matching!
##                'Trail_xyz' will be matched by 'Trail'
## requirements:  KML-structure with Placemarks containing a <name> and a <coordinates> tag
## author:        Kay Cichini
## date:          01-05-2014
## license:       CC-BY-NC-SA

ReverseKmlPath <- function(PATH_TO_DOC, NAMES) {
    
    require(XML)

    doc <- xmlInternalTreeParse(PATH_TO_DOC)
    
    if (xmlNamespaceDefinitions(doc)[[1]]$uri == "http://www.opengis.net/kml/2.2") {
        namespaces <- c(kml = "http://www.opengis.net/kml/2.2")
        flag <- 1
    } else {
        if (xmlNamespaceDefinitions(doc)[[1]]$uri == "http://earth.google.com/kml/2.0") { 
                namespaces <- c(kml0 = "http://earth.google.com/kml/2.0")
                flag <- 0
            } else {
                stop ("Stopped!: Check namespace issue..")
            }
    }
        
    
    for (NAME in NAMES) {
        
        if (flag) { 
              query <- paste0("//kml:Placemark[contains(kml:name,'", sprintf("%s", NAME), "'", ")]//kml:coordinates")
          } else {
              query <- paste0("//kml0:Placemark[contains(kml0:name,'", sprintf("%s", NAME), "'", ")]//kml0:coordinates")
          }

        coords <- tryCatch(getNodeSet(doc, query, namespaces), 
                           error = function(e) message(paste("\nError: *", NAME, "* was NOT successfully matched\n")))
        
        for (i in length(coords)) {

            #grab coordinates from node and reverse order
            rev_coord_vector <- rev(unlist(strsplit(gsub("\\t|\\n", "", xmlValue(coords[[i]])), "\\s")))
            rev_coord_string <- paste(rev_coord_vector, collapse = " ")

            # re-insert reversed line-string:
            xmlValue(coords[[i]]) <- rev_coord_string

            # message
            if (flag) { 
                  query <- paste0("//kml:Placemark[contains(kml:name,'", sprintf("%s", NAME), "'", ")]//kml:name")
              } else {
                  query <- paste0("//kml0:Placemark[contains(kml0:name,'", sprintf("%s", NAME), "'", ")]//kml0:name")
            }
            match <- xmlValue(getNodeSet(doc, query, namespaces)[[i]])
            message(paste0("matched name: ", match, "\n..."))

        }
    }

    # save:
    message("Reversed paths saved to:")
    saveXML(doc, paste0(dirname(PATH_TO_DOC), "/reversed_", basename(PATH_TO_DOC)),
            prefix = newXMLCommentNode("This file was created with the R-package XML::saveXML, see: "))
}

## not run: 
tf <- tempfile(fileext = ".kml")
download.file("http://dev.openlayers.org/releases/OpenLayers-2.13.1/examples/kml/lines.kml", tf, mode = "wb")
ReverseKmlPath( PATH_TO_DOC = tf, NAMES = c("Absolute", "Relative") )

shell.exec(tf)
shell.exec(paste0(dirname(tf), "/reversed_", basename(tf)))

ReverseKmlPath( PATH_TO_DOC = "C:/Users/Kay/Documents/Web/Openlayers/Trails/Trails.kml", 
                NAMES = c("Lanser", "Tulfein", "Magde", "Schnee" ) )


