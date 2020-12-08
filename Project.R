library(lubridate)
library(dplyr)
library(ggplot2)

#read in data
datD <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\drought.csv", 
                 stringsAsFactors = T)
datF <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\Monitoring_Trends_in_Burn_Severity_Fire_Occurrence_California.csv", 
                            na.strings=c("", "NA"), stringsAsFactors = T)
str(datD)
str(datF)

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


#create new dataframe for burn severity
burnSeverity <- data.frame(datFC$OBJECTID, datFC$HIGH_THRESHOLD, datFC$MODERATE_THRESHOLD, datFC$LOW_THRESHOLD)
colnames(burnSeverity) <- c("OBJECTID", "HIGH", "MODERATE", "LOW")


#removes no data readings
burnSeverity$HIGH <- ifelse(burnSeverity$HIGH==9999, TRUE, burnSeverity$HIGH)
burnSeverity$MODERATE <- ifelse(burnSeverity$MODERATE==9999, TRUE, burnSeverity$MODERATE)
burnSeverity$LOW <- ifelse(burnSeverity$LOW==9999, TRUE, burnSeverity$LOW)
#calculate maximum severity
burnSeverity[, "burn_threshold"] <- apply(burnSeverity[,2:4], 1, max)
#add burnSeverity to fire dataset
datFS <- inner_join(datFC, burnSeverity, by=c("OBJECTID"="OBJECTID"))


#add drought data to fire data based on county
datCounty <- inner_join(datFS, datD, by=c("COUNTY"="Name", "yearF"="yearD", "monthF"="monthD"))

#Mode Function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#calculate average USDMLevelID per COUNTY per monthF
DMonthMode <- aggregate(datCounty$USDMLevelID, by=list(datCounty$COUNTY, datCounty$monthF, datCounty$yearF), FUN=Mode)

#rename columns
colnames(DMonthMode) <- list("COUNTY", "Month", "Year", "DMode")

#add drought avg to fire data
datAllMode <- left_join(datCounty, DMonthMode, by=c("COUNTY"="COUNTY", "monthF"="Month", "yearF"="Year"))


#Clean up drought dataset by selecting wanted columns and removing duplicated
datDModeClean <- distinct(select(datAllMode, FIRE_NAME, ACRES, COUNTY, dateF, yearF, monthF, State, USDMLevelID, USDMLevel, DMode, burn_threshold))
head(datDModeClean)


#Whisker plot of acres by drought category
datDdmPlot <- as.factor(datDModeClean$DMode)
ggplot(data=datDModeClean, aes(datDdmPlot, ACRES)) + geom_boxplot() + 
  labs(title = "Acres Burned by Drought Category", x = "USDM", y = "Acres Burned")

#whisker plot of burn_severity by drought category
ggplot(data=datDModeClean, aes(datDdmPlot, burn_threshold)) + geom_boxplot() +
  labs(title = "Burn Severity by Drought Category", x = "USDM", y = "Burn Severity (dNBR)")

#plot of burn severity by acerage
#plot(datDModeClean$burn_threshold, datDModeClean$ACRES, xlab = "Burn Threshold", 
     #ylab = "Acres Burned", main="Acres Burned by Burn Threshold")



