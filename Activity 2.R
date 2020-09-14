#QUESTION 1:
#read in weather file data
datW <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\noaa_weather\\2011124.csv")

#reformatting date column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create date year column
datW$year <- as.numeric(format(datW$dateF, "%Y"))


#QUESTION 2
charVec <- c("ahs", "a", "b", "hello")
numVec <- c(1, 24, 224, 7)
intVec <- c(2L, 24L, 78L, 10L)
facVec <- factor(c("one", "two", "two", "one"))


#Calculating Mean
levels(datW$NAME)
#mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"], na.rm=TRUE)

#Calculate Average Daily Temp
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#Aggregate - faster than mean
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean", na.rm=TRUE)
#averageTemp

#Change the output of column names to more meaningful
colnames(averageTemp) <- c("NAME", "MAAT")
averageTemp

#convert factor Level to number
datW$siteN <- as.numeric(as.factor(datW$NAME))


#print all histograms
par(mfrow=c(2,2))


#QUESTION 3
#histogram for first site
hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")

#add a mean line
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
       col = "tomato3",
       lwd = 3)

#add a standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
      col = "tomato3",
      lty = 3,
      lwd =3)

#add a sd line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
       col = "tomato3",
       lty = 3,
       lwd = 3)

#QUESTION 4

#histogram for second site
hist(datW$TAVE[datW$siteN == 2],
     freq = FALSE,
     main = paste(levels(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "slategray1",
     border = "white")
#add a mean line
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
       col = "deeppink",
       lwd = 3)
#add a standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2], na.rm=TRUE),
       col = "deeppink",
       lty = 3,
       lwd =3)
#add a sd line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2], na.rm=TRUE),
       col = "deeppink",
       lty = 3,
       lwd = 3)

#histogram for 3rd site
hist(datW$TAVE[datW$siteN == 3],
     freq = FALSE,
     main = paste(levels(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "tan1",
     border = "white")
#add a mean line
abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm = TRUE),
       col = "mediumspringgreen",
       lwd = 3)
#add a standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3], na.rm=TRUE),
       col = "mediumspringgreen",
       lty = 3,
       lwd =3)
#add a sd line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3], na.rm=TRUE),
       col = "mediumspringgreen",
       lty = 3,
       lwd = 3)

#histogram for 4th site
hist(datW$TAVE[datW$siteN == 4],
     freq = FALSE,
     main = paste(levels(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "orchid1",
     border = "white")
#add a mean line
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm = TRUE),
       col = "royalblue4",
       lwd = 3)
#add a standard deviation line below the mean
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4], na.rm=TRUE),
       col = "royalblue4",
       lty = 3,
       lwd =3)
#add a sd line above the mean
abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4], na.rm=TRUE),
       col = "royalblue4",
       lty = 3,
       lwd = 3)

