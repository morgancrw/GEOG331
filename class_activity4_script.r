#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length [1] x width [2]
#2. iris  petal length [3] x width [4]
#3. iris sepal length [1] x petal length [3]

#select flower species
flower <- iris[iris$Species == "versicolor",]
#create index values for for loop
vary <- c(flower[1], flower [3], flower[1])
varx <- c(flower[2], flower[2], flower[4])

rt <- list()

#create regressions
for (i in 1:3){
  rt[[i]] <- lm(vary[[i]] ~ varx[[i]])
}


#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))

#add height col to iris dataframe and save it as a new copy
iris1 <- left_join(iris, height)

#str(iris1)



#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
#make empty plot
p <- ggplot(data = iris, mapping = aes(x=Sepal.Length, y=Sepal.Width)) 
#add points
p + geom_point()

#3b. make a scatter plot with ggplot and get rid of  busy grid lines
p + theme_classic()

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
p + geom_point(aes(colour = Species, size = 1))


#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################	

#plot will plot the x and y values for you, however, with ggplot, you are able
#to customize your graph much more with colors, shapes, and sizes. Ggplot arugments
#are also less intuitive than plot arguments -- for example you must use aes() 
#and not simply just the x and y values
