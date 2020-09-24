#install lubridate package
#install.packages(c("lubridate"))

#create assert function
# assert <- function(statement, err.message) {
#   if(statement == FALSE){
#     print(err.message)
#   }
# }

#testing assert function
#false statement
# assert(1==2, "error: unequal values")
#true statement
# assert(2==2, "error: unequal values")
#check vector length
# a <- c(1,2,3,4)
# b <- c(8,4,5)
# assert(length(a)==length(b), "error: unequal length")

#read in data file
datW <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\bewkes_weather.csv", 
                 na.strings = c('#N/A'), skip = 3, header=FALSE)
#preview data
#print(datW[1,])

#get sensor info from file
sensorInfo <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\bewkes_weather.csv",
                       na.strings=c("#N/A"), nrows=2)
#print(sensorInfo)

#get column names from sensorInfo table and set weather station colnames
colnames(datW) <- colnames(sensorInfo)
#preview data
#print(datW[1,])

#convert to m/d/y
dates <- mdy_hm(datW$timestamp, tz = "America/New_York")
#calculate day of year
datW$doy <- yday(dates)
#calculate hour in the day
datW$hour <- hour(dates) + (minute(dates)/60)
#calculate decimal day of year
datW$DD <- datW$doy + (datW$hour/24)

# #find number of missing values for air temp
# length(which(is.na(datW$air.temperature)))
# #find number of missing values for wind speed
# length(which(is.na(datW$wind.speed)))
# #find number of missing values for precipitation
# length(which(is.na(datW$precipitation)))
# #find number of missing values for soil temperature
# length(which(is.na(datW$soil.moisture)))
# #find number of missing values for soil moisture
# length(which(is.na(datW$soil.temp)))


#plot soil moisture
# plot(datW$DD, datW$soil.moisture, pch=19, type="b", xlab="Day of Year", 
#      ylab="Soil moisture (cm3 water per cm3 soil)")

#plot air temp
# plot(datW$DD, datW$air.temperature, pch=19, type="b", xlab="Day of Year",
#      ylab = "Air temperature (degrees C)") 

#new col for QA/QC
datW$air.tempQ1 <- ifelse(datW$air.temperature < 0, NA, datW$air.temperature)

#check values at the extreme range of the data
#quantile(datW$air.tempQ1)

#look at days with really low air temp
datW[datW$air.tempQ1 < 8,]
#look at days with really high air temp
datW[datW$air.tempQ1 > 33,]


