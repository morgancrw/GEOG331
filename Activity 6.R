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

#reproject the glaciers
#use the NDVI projection
g1966p <- spTransform(g1966,NDVIraster[[1]]@crs)
g1998p <- spTransform(g1998,NDVIraster[[1]]@crs)
g2005p <- spTransform(g2005,NDVIraster[[1]]@crs)
g2015p <- spTransform(g2015,NDVIraster[[1]]@crs)

#------------------------QUESTION 4---------------------
# #map maximum NDVI and glaciers in 2015
plot(NDVIraster[[13]], axes=FALSE)
plot(g2015p, border="black", add=TRUE)
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

#new data frame for % area change
gChange <- data.frame("Name"=gAll$GLACNAME)
gChange$a1966m.sq <- gAll$a1966m.sq
gChange$a2015m.sq <- gAll$a2015m.sq

#function to calculate % area change
pct <- function(original, new) {
  diff <- new - original 
  change <- diff / original
  result <- change * 100
}

gChange$areaC <- pct(gChange$a1966m.sq, gChange$a2015m.sq)

#format %change to 2 decimal places
gChange$areaC <- sprintf(gChange$areaC, fmt = '%#.2f')
gChange$areaC <- as.numeric(gChange$areaC)

g2015$change <- gChange$areaC

spplot(g2015, "change")


#----------------------------------------------------

#-------------------QUESTION 6--------------------------

#find glacier with largest % loss
change <- gChange$Name[gChange$areaC == min(gChange$areaC)]
percChange <- min(gChange$areaC)
newMap1966 <- subset(g1966p, g1966p$GLACNAME==change)
newMap1998 <- subset(g1998p, g1966p$GLACNAME==change)
newMap2005 <- subset(g2005p, g1966p$GLACNAME==change)
newMap2015 <- subset(g2015p, g1966p$GLACNAME==change)


par(mai=c(1,1,1,1))
plotRGB(rgbL, stretch="lin", axes=TRUE, main=paste("% change:", percChange, "% Glacier: ", change))
plot(newMap1966, col="lightblue2", add=TRUE, border=NA)
plot(newMap1998, col="green", add=TRUE, border=NA)
plot(newMap2005, col="mediumorchid1", add=TRUE, border=NA)
plot(newMap2015, col="grey1", add=TRUE, border=NA)


#-------------------------------------------------------
#------------------QUESTION 7------------------------
# #designate that NDVIraster list is a stack
  NDVIstack <- stack(NDVIraster)
# #
#set up lm function to apply to every cell
  timeT <- ndviYear
  fun <- function(x) {
   if(is.na(x[1])){
      NA}else{
        #fit a regression and extract a slope
        lm(x ~ timeT)$coefficients[2] }}
# 
# # #apply the slope function to the rasters
NDVIfit <- calc(NDVIstack,fun)

#plot the change in NDVI
plot(NDVIfit, axes=FALSE)
#------------------------------------------------------
#-----------------------QUESTION 8-------------------------
# #buffer glaciers
 glacier500m <- gBuffer(g1966p,
                       byid=TRUE,
                        width=500)
# #convert to a raster
buffRaster <- rasterize(glacier500m,
                         NDVIraster[[1]],
                         field=glacier500m@data$GLACNAME,
                         background=0)
plot(buffRaster)
# 
# #rasterize gralciers
glacRaster <- rasterize(g1966p, NDVIraster[[1]], field=g1966p@data$GLACNAME, background=0)
# 
# #subtract buffer from original glacier
glacZones <- buffRaster - glacRaster
plot(glacZones)

#------------------------------------------------------------
#-----------------------QUESTION 9--------------------------------
meanChange <- zonal(NDVIfit, glacZones, "mean")
g2015p@data$zmean<-meanChange[-(1),]

#add means to glacier polygons
g2015p@data$zmean <- 0

for(i in 1:39) {
  g2015p@data$zmean[i] = meanChange[i+1,2]
}

spplot(g2015p, "zmean")


#------------------------------------------------------
#--------------------QUESTION 11------------------------
rmean <- calc(NDVIstack, mean)

NDVImean <- list()
for (i in 1:length(ndviYear)){
  NDVImean[i] <- cellStats(NDVIraster[[i]], stat="mean", na.rm=TRUE)
}

mean(unlist(NDVImean))

#find NDVI range
NDVIfitmin <- calc(NDVIstack, min)
NDVIfitmax <- calc(NDVIstack, max)
NDVIfitrange <- NDVIfitmax - NDVIfitmin

#find NDVI range per polygon
averange <- raster::extract(NDVIfitrange, g2015p, buffer=0, df=TRUE)
g2015p@data$averange1 <- 0
for (i in 1:39){
  g2015p@data$averange1[i]=averange[i,2]
}

#plot NDVI color coated polygons
spplot(g2015p, "averange1")

#plot NDVI average
plot(rmean)

#-------------------------------------------------------
