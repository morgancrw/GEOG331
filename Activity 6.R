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


#fix glacier name so that it is consisten with the enitre time period
g2015@data$GLACNAME <- ifelse(g2015@data$GLACNAME == "North Swiftcurrent Glacier",
                              "N. Swiftcurrent Glacier",
                              ifelse(   g2015@data$GLACNAME ==  "Miche Wabun", 
                                        "Miche Wabun Glacier",
                                        as.character(g2015@data$GLACNAME)))

#read in rgb imagery from landsat
redL <- raster("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\glacier_09_05_14\\l08_red.tif")
greenL <- raster("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\glacier_09_05_14\\l08_green.tif")
blueL <- raster("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\glacier_09_05_14\\l08_blue.tif")

#make a brick that stacks all layers
rgbL <- brick(redL, greenL, blueL)

#read in NDVI files
#set up years to read in
ndviYear <- seq(2003,2016)
#read all files into a list
NDVIraster <- list() 
for(i in 1:length(ndviYear)){
  NDVIraster[[i]] <- raster(paste0("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\data\\NDVI\\NDVI_",ndviYear[i],".tif"))
  
}

#---------------------------QUESTION 3---------------------------
#plot both side by side
par(mfrow=c(1,2))
#plot with color
#show axes for reference
#add contrast to the imagery to see it better
par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE)
#add polygons to plot
plot(g1966, col="tan3", border=NA, add=TRUE)
plot(NDVIraster[[1]])
#---------------------------------------------------------------