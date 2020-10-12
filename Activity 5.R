#load in lubridate
library(lubridate)

#read in streamflow data
datH <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\stream_flow_data.csv", na.strings = c("Eqp"))


#read in precipitation data
datP <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\2049867.csv")
nrow(datP)

#only use most reliable measurements
datD <- datH[datH$discharge.flag == "A",]

######define time for streamflow######
#convert date and time
datesD <- as.Date(datD$date, "%m/%d/%Y")
#get day of year
datD$doy <- yday(datesD)
#calculate year
datD$year <- year(datesD)
#define time
timesD <- hm(datD$time)

####define time for precipitation#####
dateP <- ymd_hm(datP$DATE)
#get day of year
datP$doy <- yday(dateP)
#get year
datP$year <- year(dateP)


###get decimal formats###
#convert time from a string to a more usable format
#with a decimal hour
datD$hour <- hour(timesD ) + (minute(timesD )/60)
#get full decimal time
datD$decDay <- datD$doy + (datD$hour/24)
#calculate a decimal year, but account for leap year
datD$decYear <- ifelse(leap_year(datD$year), datD$year + (datD$decDay/366),
                       datD$year + (datD$decDay/365))

#calculate times for datP
datP$hour <- hour(dateP ) + (minute(dateP )/60)
#get full decimal time
datP$decDay <- datP$doy + (datP$hour/24)
#calculate a decimal year, but account for leap year
datP$decYear <- ifelse(leap_year(datP$year), datP$year + (datP$decDay/366), 
                       datP$year + (datP$decDay/365))

#basic plot formatting
aveF <- aggregate(datD$discharge, by=list(datD$doy), FUN="mean")
colnames(aveF) <- c("doy", "dailyAve")
sdF <- aggregate(datD$discharge, by=list(datD$doy), FUN="sd")
colnames(sdF) <- c("doy", "dailySD")

#start new plot
dev.new(width=8, height=8)
#bigger margins
par(mai=c(1,1,1,1))
#make plot
plot(aveF$doy, aveF$dailyAve,
     type="l",
     xlab="Month",
     ylab=expression(paste("Discharge ft"^"3 ", "sec"^"-1")),
     lwd=2,
     ylim=c(0,180),
     xaxs="i", yaxs="i", #remove gaps from axes
     axes=FALSE)#no axes 

#show standard deviation around the mean
polygon(c(aveF$doy, rev(aveF$doy)), #x coordinates
        c(aveF$dailyAve-sdF$dailySD, rev(aveF$dailyAve+sdF$dailySD)),# y coordinates
        col=rgb(0.392, 0.584, 0.929, .2), #color that is semi-transparent
        border=NA #no border
        )


#--------------QUESTION 5------------------
#creat dataframe with only data from 2017
obsYear <- data.frame(datD$discharge[datD$year==2017])
obsYear$doy <- datD$doy[datD$year==2017]
colnames(obsYear) <- c("discharge", "doy")

#new axis display with ticks for each month
axis(1, seq(0,360, by=30), #tick intervals
     lab=seq(0,12, by=1)) #tick labels
axis(2, seq(0,180, by=20),
     seq(0,180, by=20),
     las = 2)#show ticks at 90 degree angle

#add legend with new line info
legend("topleft", c("mean","1 standard deviation", "2017 discharge"), #legend items
       lwd=c(2,NA),#lines
       col=c("black", rgb(0.392, 0.584, 0.929, .2), "grey71"), #colors
       pch=c(NA, 15, NA),#symbols
       bty="n")#no legend border

#adding line for 2017 observations
lines(obsYear$doy, obsYear$discharge, lwd=2, col=c("grey71"))


#---------------------------------------------------------


#-----------------------QUESTION 7------------------------





