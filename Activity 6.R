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
# #plot both side by side
# par(mfrow=c(1,2))
# #plot with color
# #show axes for reference
# #add contrast to the imagery to see it better
# par(mai=c(1,1,1,1))
# plotRGB(rgbL, stretch="lin", axes=TRUE)
# #add polygons to plot
# plot(g1966, col="tan3", border=NA, add=TRUE)
# plot(NDVIraster[[1]])
#---------------------------------------------------------------

#reproject the glaciers
#use the NDVI projection
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#------------------------QUESTION 4---------------------
# #map maximum NDVI and glaciers in 2015
# plot(NDVIraster[[13]], axes=FALSE)
# plot(g2015p, border="black", add=TRUE)
#-------------------------------------------------------

#calculate area for all polygons
#add directly into data table for each shapefile
g1966p@data$a1966m.sq <- area(g1966p)
g1998p@data$a1998m.sq <- area(g1998p)
g2005p@data$a2005m.sq <- area(g2005p)
g2015p@data$a2015m.sq <- area(g2015p)

#join glacier data into new table
gAllp1 <- join(g1966p@data,g1998p@data, by="GLACNAME", type="full")
gAllp2 <- join(gAllp1,g2005p@data, by="GLACNAME", type="full")
gAll <- join(gAllp2,g2015p@data, by="GLACNAME", type="full")


#---------------------QUESTION 5---------------------

# %change = (difference / original) * 100

head(gAll)


#----------------------------------------------------

