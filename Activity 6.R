#install spatial packages
#install.packages(c("raster","sp","rgdal","rgeos","plyr"))

#load packages
library(raster)
library(sp)
library(rgdal)
library(rgeos)
library(plyr)

#read in shapefiles
g1966 <- readOGR("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\GNPglaciers\\GNPglaciers_1966.shp", stringsAsFactors = T)
g1998 <- readOGR("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\GNPglaciers\\GNPglaciers_1998.shp", stringsAsFactors = T)
g2005 <- readOGR("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\GNPglaciers\\GNPglaciers_2005.shp", stringsAsFactors = T)
g2015 <- readOGR("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\GNPglaciers\\GNPglaciers_2015.shp", stringsAsFactors = T)

str(g2015)