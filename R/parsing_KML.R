library(XML)

# new kml file... needs to be well-formed
z <-
  '<kml xmlns="http://www.opengis.net/kml/2.2">
      <Document>
         <Folder>
            <name>ROUTES</name>
         </Folder>
      </Document>
    </kml>'
new_xmlDoc <- xmlInternalTreeParse(z, useInternalNodes = TRUE)

# important add all namespace definitions...
ns <- c(gx="http://www.google.com/kml/ext/2.2",
        kml="http://www.opengis.net/kml/2.2",
        atom="http://www.w3.org/2005/Atom")
ensureNamespace(new_xmlDoc, ns)

# get the root off the new file for latter processing
new_root <- xmlRoot(new_xmlDoc)

# loop over files from folder
# and insert Placemark content of each file as children nodes into 
# the new file

setwd("C:/Users/Kay/Google Drive/SKI-BIKE/Gastein")
files <- dir(pattern="bergfex*")

for (f in files) { 
   
   # get placemark node of each file
   doc <- xmlInternalTreeParse(f, useInternalNodes = TRUE)
   root <- xmlRoot(doc)
   plcm_node <- root[["Document"]][["Folder"]][["Folder"]][["Placemark"]]

   # insert file name as Placemark name
   xmlValue(plcm_node[["name"]]) <- sub('bergfextour_(.*)[.]kml', '\\1', f)

   # add placemark node to new doc
   addChildren(new_root[["Document"]][["Folder"]], plcm_node)

}

# save it...
saveXML(new_xmlDoc, "ROUTE_collection.kml")

