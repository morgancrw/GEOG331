library(lubridate)
library(dplyr)

#read in data
datD <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\drought.csv", 
                 stringsAsFactors = T)
datF <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\Monitoring_Trends_in_Burn_Severity_Fire_Occurrence_California.csv", 
                            na.strings=c("", "NA"), stringsAsFactors = T)


#create new fire dataset for only fires with county names
datFC <- subset(datF, datF$COUNTY != "NA")

#edit date format so they are the same in each data set
datFC$dateF <- as.Date(datFC$IG_DATE)
datD$dateD <- as.Date(datD$MapDate)
#make year columns
datFC$yearF <- year(datFC$dateF)
datD$yearD <- year(datD$dateD)
#make month columns
datFC$monthF <- month(datFC$dateF)
datD$monthD <- month(datD$dateD)


#add drought data to fire data based on county and date
datCounty <- inner_join(datFC, datD, by=c("COUNTY"="Name", "yearF"="yearD", "monthF"="monthD"))

head(datCounty)





