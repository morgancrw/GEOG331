
#read in weather file data
datW <- read.csv("C:\\Users\\morga\\OneDrive\\Documents\\GitHub\\GEOG331\\noaa_weather\\2011124.csv", 
                 stringsAsFactors = T)

#reformatting date column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

#create date year column
datW$year <- as.numeric(format(datW$dateF, "%Y"))


#----------------QUESTION 2-------------------
charVec <- c("ahs", "a", "b", "hello")
numVec <- c(1, 24, 224, 7)
intVec <- c(2L, 24L, 78L, 10L)
facVec <- factor(c("one", "two", "two", "one"))
#---------------------------------------------

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


#-------------------QUESTION 3-------------------------
# #histogram for first site
h1 <- hist(datW$TAVE[datW$siteN == 1],
     freq = FALSE,
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)",
     ylab = "Relative frequency",
     col = "grey50",
     border = "white")
# 
# #add a mean line
# abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
#        col = "tomato3",
#        lwd = 3)
# 
# #add a standard deviation line below the mean
# abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
#       col = "tomato3",
#       lty = 3,
#       lwd =3)
# 
# #add a sd line above the mean
# abline(v = mean(datW$TAVE[datW$siteN == 1], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE),
#        col = "tomato3",
#        lty = 3,
#        lwd = 3)
#---------------------------------------------------------


#----------------------QUESTION 4--------------------------

# #histogram for second site
# hist(datW$TAVE[datW$siteN == 2],
#      freq = FALSE,
#      main = paste(levels(datW$NAME)[2]),
#      xlab = "Average daily temperature (degrees C)",
#      ylab = "Relative frequency",
#      col = "slategray1",
#      border = "white")
# #add a mean line
# abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm = TRUE),
#        col = "deeppink",
#        lwd = 3)
# #add a standard deviation line below the mean
# abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2], na.rm=TRUE),
#        col = "deeppink",
#        lty = 3,
#        lwd =3)
# #add a sd line above the mean
# abline(v = mean(datW$TAVE[datW$siteN == 2], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2], na.rm=TRUE),
#        col = "deeppink",
#        lty = 3,
#        lwd = 3)
# 
# #histogram for 3rd site
# hist(datW$TAVE[datW$siteN == 3],
#      freq = FALSE,
#      main = paste(levels(datW$NAME)[3]),
#      xlab = "Average daily temperature (degrees C)",
#      ylab = "Relative frequency",
#      col = "tan1",
#      border = "white")
# #add a mean line
# abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm = TRUE),
#        col = "mediumspringgreen",
#        lwd = 3)
# #add a standard deviation line below the mean
# abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3], na.rm=TRUE),
#        col = "mediumspringgreen",
#        lty = 3,
#        lwd =3)
# #add a sd line above the mean
# abline(v = mean(datW$TAVE[datW$siteN == 3], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3], na.rm=TRUE),
#        col = "mediumspringgreen",
#        lty = 3,
#        lwd = 3)
# 
# #histogram for 4th site
# hist(datW$TAVE[datW$siteN == 4],
#      freq = FALSE,
#      main = paste(levels(datW$NAME)[4]),
#      xlab = "Average daily temperature (degrees C)",
#      ylab = "Relative frequency",
#      col = "orchid1",
#      border = "white")
# #add a mean line
# abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm = TRUE),
#        col = "royalblue4",
#        lwd = 3)
# #add a standard deviation line below the mean
# abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4], na.rm=TRUE),
#        col = "royalblue4",
#        lty = 3,
#        lwd =3)
# #add a sd line above the mean
# abline(v = mean(datW$TAVE[datW$siteN == 4], na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4], na.rm=TRUE),
#        col = "royalblue4",
#        lty = 3,
#        lwd = 3)
#---------------------------------------------------

#--------------------QUESTION 5---------------------
#calculating the probability density and adding the normal distribution curve

#normal distribution curve for location 1
x.plot <- seq(-10, 30, length.out=100)
y.plot <- dnorm(seq(-10,30, length.out=100), 
                mean(datW$TAVE[datW$siteN ==1], na.rm = TRUE),
                sd(datW$TAVE[datW$siteN == 1], na.rm = TRUE))
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot
points(x.plot,
       y.scaled,
       type = "l",
       col = "royalblue3",
       lwd = 4,
       lty = 2)
#---------------------------------------------------

#calculating probability
pnorm(0,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#calculating probability of temperature below 5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#calculating probability of temperatures in the range of 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#calculating the probability of temperatures above 20
1 - pnorm(20,
         mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
         sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#calculating the value of 95%
qnorm(.95,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE),
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))

#------------------QUESTION 6----------------------
1- pnorm(18.51026,
      mean(datW$TAVE[datW$siteN == 1], na.rm = TRUE) + 4,
      sd(datW$TAVE[datW$siteN == 1], na.rm=TRUE))


       