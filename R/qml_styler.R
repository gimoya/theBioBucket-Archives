# Author: Kay Cichini
# Date: 15-05-2013
# Name: qml-styler.R
# Purpose: make a QGIS-style in XML-format using a text file with legend data
# data source: 
# legend: http://eusoils.jrc.ec.europa.eu/ESDB_Archive/ESDBv3/legend/sg_attr.htm
# vector data (to which style is applied): http://eusoils.jrc.ec.europa.eu/library/esdac/OnLine_Data.cfm 

require(RColorBrewer)
require(XML)
 
tf <- read.delim("D:/GIS_DataBase/Environmental_Data/soil/vector/FAO90-LEV1.txt",
                 sep = "\t", header = T, stringsAsFactors = F)

# here's the data with dput: 
# > dput(tf)
tf <- structure(list(Value = c("AC", "AL", "AN", "AR", "AT", "CH", 
"CL", "CM", "FL", "FR", "GL", "GR", "GY", "HS", "KS", "LP", "LV", 
"LX", "NT", "PD", "PH", "PL", "PT", "PZ", "RG", "SC", "SN", "VR", 
"1", "2", "3", "4", "5", "6"), Label = c("Acrisol", "Alisol", 
"Andosol", "Arenosol", "Anthrosol", "Chernozem", "Calcisol", 
"Cambisol", "Fluvisol", "Ferralsol", "Gleysol", "Greyzem", "Gypsisol", 
"Histosol", "Kastanozem", "Leptosol", "Luvisol", "Lixisol", "Nitisol", 
"Podzoluvisol", "Phaeozem", "Planosol", "Plinthosol", "Podzol", 
"Regosol", "Solonchak", "Solonetz", "Vertisol", "Town", "Soildisturbedbyman", 
"Waterbody", "Marsh", "Glacier", "Rockoutcrops")), .Names = c("Value", 
"Label"), class = "data.frame", row.names = c(NA, -34L))

head(tf)
#  Value     Label
#1    AC   Acrisol
#2    AL    Alisol
#3    AN   Andosol
#4    AR  Arenosol
#5    AT Anthrosol
#6    CH Chernozem

n <- nrow(tf)
 
# random colors
rand_rgb <- function() paste(paste(sample(1:255, 3, replace=T), collapse=","), "255", sep=",")
for(i in 1:n) tf$v[i] <- rand_rgb()
 
# set alpha
alpha = 0.25

# make the XML:
# note that you have to check for the attribute name from the table and set it here:
attribute = "FAO90LV1"
 
base = newXMLNode("qgis")
addAttributes(base,version="1.8.0-Lisboa",minimumScale="0",maximumScale="1e+08",hasScaleBasedVisibilityFlag="0")
trans <- newXMLNode("transparencyLevelInt", 255)
rend <- newXMLNode("renderer-v2", attrs = c(attr=attribute,symbollevels="0",type="categorizedSymbol"))
 
# sort the categories
categories <- newXMLNode("categories")
category <- lapply(seq_along(tf$Value),function(x){newXMLNode("category",
 attrs = c(symbol = as.character(x-1), value = tf$Value[x], label = tf$Label[x]))
 })
addChildren(categories,category)
 
# sort the symbols
symbols <- newXMLNode("symbols")
symbol <- lapply(seq_along(tf$Value),function(x){dum.sym <- newXMLNode("symbol",
 attrs = c(outputUnit="MM",alpha=alpha,type="fill",name=as.character(x-1)))
 layer <- newXMLNode("layer", attrs =c(pass="0",class="SimpleFill",locked="0"))
 prop <- newXMLNode("prop", attrs =c(k="color",v= tf$v[x]))
 addChildren(layer, prop)
 addChildren(dum.sym, layer)
 })
 
addChildren(symbols, symbol)
 
# add categories and symbols to rend
addChildren(rend, list(categories, symbols))
addChildren(base, list(trans, rend))
 
# save to qml-file
writeLines(saveXML(base), "D:/GIS_DataBase/Environmental_Data/soil/vector/FAO90-LEV1.qml")