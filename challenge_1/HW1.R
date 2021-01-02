data <- read.csv("forestfires.csv")
str(data)
data$month <- as.numeric(data$month)

#a
par(mfrow=c(2,2)) 
plot(data$temp,data$area, xlab = "Temperature",ylab = "Area", main = "Area Vs Temp" ) 
plot(data$month,data$area, xlab = "Month",ylab = "Area", main = "Area Vs Month" ) 
plot(data$DC,data$area, xlab = "DC",ylab = "Area", main = "Area Vs DC" ) 
plot(data$RH,data$area, xlab = "RH",ylab = "Area", main = "Area Vs RH" )

#b
hist(data$wind, freq = TRUE, main="Wind Speed Distribution", xlab = "Wind Speed in Km/hr",ylim = c(0,150))

#c
summary(data$wind)

#d
hist(data$wind, freq = FALSE, main="Wind Speed Distribution", xlab = "Wind Speed in Km/hr",ylim = c(0,0.30)) 
lines(density(data$wind),col="Blue",lwd=2)

#e
par(mfrow=c(2,1))
d <- density(data$month)
plot(d)
d <- density(data$month)
plot(d, main="Kernel Density of Miles Per Gallon")
polygon(d, col="red", border="blue")
rug(data$month, col="brown")

#f
#install.packages("GGally")
#install.packages("ggplot2")
library(GGally)
library(ggplot2)
ggpairs(data[,c(9,10,7,6)])


#g
par(mfrow=c(1,3)) 
boxplot(data$wind,main="Wind Distribution") 
boxplot(data$ISI,main="ISI Distribution") 
boxplot(data$DC,main="DC Distribution")

#h
par(mfrow=c(1,2)) 
hist(data$DMC,main = "Distribution of DMC",xlab = "DMC",ylim = c(0,100)) 
hist(log(data$DMC),main = "Distribution of log(DMC)", xlab = "log(DMC)", ylim = c(0,250))

par(mfrow=c(1,2)) 
hist(data$DMC,main = "Distribution of DMC",xlab = "DMC",freq = FALSE) 
lines(density(data$DMC),col="Red",lwd=2) 
hist(log(data$DMC),main = "Distribution of log(DMC)", xlab = "log(DMC)",freq = FALSE) 
lines(density(log(data$DMC)),col="Red",lwd=2)

#2
#a
data2 <- read.csv("M01_quasi_twitter.csv")

par(mfrow=c(1,1)) 
hist(log(data2$friends_count),freq = FALSE, main="Distribution of Friends_Count", xlab = "Friends_Count")

#b
summary(data2$friends_count)

#c
a<- sum(is.na(data2$friends_count)) 
a

#d
#install.packages("scatterplot3d")
require("scatterplot3d")
scatterplot3d(data2$created_at_year , data2$education , data2$age )

#e
#pie3D(Accounts,labels = labls,explode = 0.1,main="3D Pie Chart",radius = 0.9,start = 0.785 ) 
#install.packages("plotrix")
library(plotrix) 
Countries <- c("UK","Canada","India","Australia","USA") 
Accounts <- c(650,1000,900,300,14900) 
percentage <- round(Accounts/sum(Accounts)*100) 
labls <- paste(Countries," ",percentage,"%",sep = " ") 
par(mfrow= c(1,2)) > pie(Accounts,labels = labls,col = rainbow(length(Countries)),main = "Pie Chart with Percentage") 
pie3D(Accounts,labels = labls,explode = 0.1,main="3D Pie Chart",radius = 0.9,start = 0.785 )

#f
plot(density(data2$created_at_year), main ="Kernal Density Plot for Created at Year")

#3
#a
data3<- read.csv("raw_data.csv") 
Ndata <- scale(data3)

#b
boxplot(data3, main=" Original Data")

#c
boxplot(Ndata, main=" Normalized Data")

#d

#e
plot(data3$A,data3$B, xlab = "A",ylab = "B")