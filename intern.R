#clearing global environment
rm(list=ls())

#setting working directory
setwd("C:/Users/Admin/Desktop/intern2")

#loading data
library(readxl)
mydata<-read_excel('ENB2012_data.Xlsx')

#dimensions of the data
dim(mydata)#8 independent and 2 respnose variables

#renaming columns
library(dplyr)
names(mydata)
newdata=rename(mydata,Relative_Compactness=X1,
Surface_Area=X2, 
Wall_Area=X3,
Roof_Area=X4,
Overall_Height=X5, 
Orientation=X6,
Glazing_Area=X7, 
Glazing_Area_Distribution=X8,
Heating_Load=Y1,
Cooling_Load=Y2)

newdata1<-newdata
names(newdata)
str(newdata)

library(corrplot)
cor1=as.matrix(cor(newdata))
corrplot(cor1,method = "number")

#creating multiple data sets for convenience
predictors=subset(newdata,select = -c(9,10))
targetvars=subset(newdata,select = c(9,10))
predictorswidhl=subset(newdata,select = -c(10))
predictorswidcl=subset(newdata,select = -c(9))

#variance of each attribute
var(predictorswidhl$Relative_Compactness)
var(predictorswidhl$Surface_Area)
var(predictorswidhl$Wall_Area)
var(predictorswidhl$Roof_Area)
var(predictorswidhl$Overall_Height)
var(predictorswidhl$Orientation)
var(predictorswidhl$Glazing_Area)
var(predictorswidhl$Glazing_Area_Distribution)

#range
range(predictorswidhl$Relative_Compactness)
range(predictorswidhl$Surface_Area)
range(predictorswidhl$Wall_Area)
range(predictorswidhl$Roof_Area)
range(predictorswidhl$Overall_Height)
range(predictorswidhl$Orientation)
range(predictorswidhl$Glazing_Area)
range(predictorswidhl$Glazing_Area_Distribution)

#understaning data
stats<-function(x){
  table(x)
}
apply(predictors, 2, stats)

#plots
#relative compactness:is the measure of the closure of building.
#more compact the building less will be the empty area inside which needs to be heated or cooled.
boxplot(predictorswidhl$Relative_Compactness)
boxplot(predictorswidhl$Surface_Area)

par(mfrow=c(3,3))
plot(Heating_Load~.,data = predictorswidhl)
plot(Cooling_Load~.,data = predictorswidcl)

#plots of explanatory variables with heating_load(target variable)
library(ggplot2)
#relative compactness
qplot(predictorswidhl$Relative_Compactness,predictorswidhl$Heating_Load)#1
qplot(predictorswidcl$Relative_Compactness,predictorswidcl$Cooling_Load)#1
#building with less relative compactness is more energy efficient
#surface area
qplot(predictorswidhl$Surface_Area,predictorswidhl$Heating_Load)#2
qplot(predictorswidcl$Surface_Area,predictorswidcl$Cooling_Load)#2
#building with more surface area is less energy efficient

#wall_area
qplot(predictorswidhl$Wall_Area,predictorswidhl$Heating_Load)#3
qplot(predictorswidcl$Wall_Area,predictorswidcl$Cooling_Load)#3

#roof area
qplot(predictorswidhl$Roof_Area,predictorswidhl$Heating_Load)#4
qplot(predictorswidcl$Roof_Area,predictorswidcl$Cooling_Load)#4
#building with high roof area is more energy efficient

#overall height
qplot(predictorswidhl$Overall_Height,predictorswidhl$Heating_Load)#5
qplot(predictorswidcl$Overall_Height,predictorswidcl$Cooling_Load)#5

#orientation
qplot(predictorswidhl$Orientation,predictorswidhl$Heating_Load)#6
qplot(predictorswidcl$Orientation,predictorswidcl$Cooling_Load)#6

#glazing area
qplot(predictorswidhl$Glazing_Area,predictorswidhl$Heating_Load)#7
qplot(predictorswidcl$Glazing_Area,predictorswidcl$Cooling_Load)#7
hist(predictorswidcl$Glazing_Area)

#glazing area distribution
qplot(predictorswidhl$Glazing_Area_Distribution,predictorswidhl$Heating_Load)#8
qplot(predictorswidcl$Glazing_Area_Distribution,predictorswidcl$Cooling_Load)#8


#missing values
sort(colSums(is.na(predictorswidhl)),decreasing = TRUE)#no NAs

#correlation plot of variables
library(corrplot)
cor=as.matrix(cor(predictors))
corrplot(cor,method="number")


#binning target variables
newdata$Heating_Load<-round(newdata$Heating_Load)
newdata$Cooling_Load<-round(newdata$Cooling_Load)
library(infotheo)
hist(newdata$Heating_Load)
hist(newdata$Cooling_Load)
Heating_Load<-discretize(newdata$Heating_Load,"equalfreq",5)
Cooling_Load<-discretize(newdata$Cooling_Load,"equalfreq",5)
newdata<-cbind(newdata[,-c(9,10)],Heating_Load,Cooling_Load)

str(newdata)
#converting glazing area,glazing area distribution,orientation and target variables into factors
newdata[,c(6,7,8,9,10)]<-lapply(newdata[,c(6,7,8,9,10)], factor)

mean(newdata$Relative_Compactness)
median(newdata$Relative_Compactness)
sqrt(var(newdata$Relative_Compactness))
min(newdata$Relative_Compactness)
max(newdata$Relative_Compactness)

mean(newdata$Roof_Area)
median(newdata$Relative_Compactness)
sqrt(var(newdata$Roof_Area))
min(newdata$Roof_Area)
max(newdata$Roof_Area)

mean(newdata$Wall_Area)
median(newdata$Wall_Area)
sqrt(var(newdata$Wall_Area))
min(newdata$Wall_Area)
max(newdata$Wall_Area)


mean(newdata$Overall_Height)
median(newdata$Overall_Height)
sqrt(var(newdata$Overall_Height))
min(newdata$Overall_Height)
max(newdata$Overall_Height)


mean(newdata$Surface_Area)
median(newdata$Surface_Area)
sqrt(var(newdata$Surface_Area))
min(newdata$Surface_Area)
max(newdata$Surface_Area)
