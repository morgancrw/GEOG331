#install lubridate package
#install.packages(c("lubridate"))
library(lubridate)

#create assert function
 assert <- function(statement, err.message) {
   if(statement == FALSE){
     print(err.message)
   }
 }

#testing assert function
#false statement
 assert(1==2, "error: unequal values")
#true statement
 assert(2==2, "error: unequal values")
#check vector length
 a <- c(1,2,3,4)
 b <- c(8,4,5)
 assert(length(a)==length(b), "error: unequal length")

#read in data file
datW <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\bewkes_weather.csv", 
                 na.strings = c('#N/A'), skip = 3, header=FALSE)
#preview data
print(datW[1,])

#get sensor info from file
sensorInfo <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\bewkes_weather.csv",
                       na.strings=c("#N/A"), nrows=2)
print(sensorInfo)

#get column names from sensorInfo table and set weather station colnames
colnames(datW) <- colnames(sensorInfo)
#preview data
print(datW[1,])

# #convert to m/d/y
dates <- mdy_hm(datW$timestamp, tz = "America/New_York")
# #calculate day of year
datW$doy <- yday(dates)
# #calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
# #calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

# #find number of missing values for air temp
 length(which(is.na(datW$air.temperature)))
# #find number of missing values for wind speed
 length(which(is.na(datW$wind.speed)))
# #find number of missing values for precipitation
 length(which(is.na(datW$precipitation)))
# #find number of missing values for soil temperature
 length(which(is.na(datW$soil.moisture)))
# #find number of missing values for soil moisture
 length(which(is.na(datW$soil.temp)))


#plot soil moisture
 plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab="Day of Year", 
      ylab="Soil moisture (cm3 water per cm3 soil)")

#plot air temp
 plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab="Day of Year",
      ylab = "Air temperature (degrees C)") 

#new col for QA/QC
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check values at the extreme range of the data
quantile(datW$air.tempQ1)

#look at days with really low air temp
datW[datW$air.tempQ1 < 8,]
#look at days with really high air temp
datW[datW$air.tempQ1 > 33,]


# #plot precipitation and lightning strikes on the same plot
# #normalize lightning strikes to match precipitation
 lightscale <- (max(datW$precipitation)/max(datW$lightning.acvitivy)) * 
   datW$lightning.acvitivy
# #make plot with precipitation adn ligtning activity marked
# #make it empty to start and add in features
 plot(datW$DD, datW$precipitation, xlab="Day of Year", ylab="Precipitation & Lightning",
     type = "n")
# #plot semitransparent points for precipitation > 0 
 points(datW$DD[datW$precipitation > 0], datW$precipitation[datW$precipitation > 0], 
        col = rgb(95/255, 158/255, 160/255, .5), pch = 15)
# #plot lighning points where there is lightning
 points(datW$DD[lightscale > 0], lightscale[lightscale > 0], col="tomato3", pch=19)



#-------------------QUESTION 5------------------
#check if lightscale and precipitation have the same number of data points
assert(length(lightscale)==length(datW$precipitation), "Error: different lengths")
#------------------------------------------------
#------------------QUESTION 6----------------------
 #filter out storms in wind and air temperature measurements
 #filter all values with lightning that coincides with rainfall grather than 2mm 
 #       or only rainfal over 5mm
 #create a new air temp column
 datW$air.tempQ2 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0,
                           NA, ifelse(datW$precipitation > 5, NA, datW$air.tempQ1))
 #create a new windspeed column
 datW$wind.speedQ1 <- ifelse(datW$precipitation >= 2 & datW$lightning.acvitivy > 0,
                             NA, ifelse(datW$precipitation > 5, NA, datW$wind.speed))
 
#using assert to verify that the same amount of winspeed and airtemp was filtered
 assert(length(datW$air.tempQ2)==length(datW$wind.speedQ1), "Error: Different Lengths")

#plot of new windspeed data with lines 
 plot(datW$DD, datW$wind.speedQ1, pch=19, type="b", xlab="Day of Year", 
        ylab="Wind Speed")
#plot of new windspeed data with points
#create empty plot
 plot(datW$DD, datW$wind.speedQ1, xlab="Day of Year", ylab="Wind Speed",
            type = "n")
 
 #add points
points(datW$DD[datW$wind.speedQ1 > 0], datW$wind.speedQ1[datW$wind.speedQ1 > 0], col="tomato3", pch=19)
#----------------------------------------------------
#-------------------QUESTION 7 + 9-------------------
# #display 4 plots at once
  par(mfrow=c(2,2))
# #  
# # #BIG PICTURE PLOTS
# # #plot soil moisture
  plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab="Day of Year", 
             ylab="Soil moisture (cm3 water per cm3 soil)")
# # #plot soil temp
  plot(datW$DD, datW$soil.temp, pch=19, type="b", xlab="Day of Year", 
       ylab="Soil temp")
# # #plot air temp
  plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab="Day of Year", 
       ylab="Air temp")
#plot precipitation
  plot(datW$DD, datW$precipitation, pch=19, type="b", xlab="Day of Year", 
       ylab="Precipitation")
# 
# #MONTH OF JULY PLOTS
# #plot soil temp for July
 plot(datW$DD, datW$soil.temp, pch=19, type="b", xlim = c(175, 215), xlab="Day of Year", 
       ylab="Soil temp)")
# #plot air temp for July
 plot(datW$DD, datW$air.temperature, pch=19, type="b", xlim = c(175, 215), xlab="Day of Year", 
      ylab="Air temp")

#------------------------------------------------

#------------------QUESTION 8----------------------
#create data frame for researchers requested values and add total precipitation
datSummary <- data.frame("totalPrecipitation" = sum(datW$precipitation, na.rm =TRUE))
#fill data frame with average air temp, windspeed, soil moisture, and soil temp
datSummary$aveAirTemp <- mean(datW$air.temperature, na.rm = TRUE)
datSummary$aveWindSpeed <- mean(datW$wind.speed, na.rm=TRUE)
datSummary$aveSoilMoist <- mean(datW$soil.moisture, na.rm=TRUE)
datSummary$aveSoilTemp <- mean(datW$soil.temp, na.rm = TRUE)
datSummary$numObservations <- length(datW$air.temperature)
datSummary$timePeriodDD <- max(datW$DD, na.rm = TRUE)

str(datSummary)

#---------------------------------------------------



