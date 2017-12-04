#clearing global environment
rm(list=ls())

#setting working directory
setwd("C:/Users/Admin/Desktop/intern2/work")

#loading data
library(readxl)
mydata<-read_excel('dataset.xlsx')
mydata<-as.data.frame(mydata)

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

#missing values
sort(colSums(is.na(newdata)),decreasing = TRUE)

#missing value plot
library(Amelia)
missmap(newdata)
names(newdata)
#imputing missing values
library(DMwR)
newdata1<-centralImputation(newdata)
newdata2<-knnImputation(newdata,k=3)

#binning centralimputed data
#binning response variables
library(infotheo)
hist(newdata1$Heating_Load)
hist(newdata1$Cooling_Load)

Heating_Load<-discretize(newdata1$Heating_Load,"equalfreq",5)
Cooling_Load<-discretize(newdata1$Cooling_Load,"equalfreq",5)
newdata1$Heating_Load<-as.factor(Heating_Load$X)
newdata1$Cooling_Load<-as.factor(Cooling_Load$X)

levels(newdata1$Cooling_Load)
levels(newdata1$Heating_Load)

#renaming class labels
levels(newdata1$Heating_Load)[1]="verylow"
levels(newdata1$Heating_Load)[2]="low"
levels(newdata1$Heating_Load)[3]="medium"
levels(newdata1$Heating_Load)[4]="high"
levels(newdata1$Heating_Load)[5]="veryhigh"


levels(newdata1$Cooling_Load)[1]="verylow"
levels(newdata1$Cooling_Load)[2]="low"
levels(newdata1$Cooling_Load)[3]="medium"
levels(newdata1$Cooling_Load)[4]="high"
levels(newdata1$Cooling_Load)[5]="veryhigh"

#binning relativecompactness
table(newdata1$Relative_Compactness)
newdata1$Relative_Compactness[(newdata1$Relative_Compactness>0)&(newdata1$Relative_Compactness<=0.7)]<-"verylow"
newdata1$Relative_Compactness[(newdata1$Relative_Compactness>0.7)&(newdata1$Relative_Compactness<=0.8)]<-"low"
newdata1$Relative_Compactness[(newdata1$Relative_Compactness>0.8)&(newdata1$Relative_Compactness<0.9)]<-"high"
newdata1$Relative_Compactness[(newdata1$Relative_Compactness>=0.9)&(newdata1$Relative_Compactness<=1)]<-"veryhigh"

#binning surface area
table(newdata1$Surface_Area)
newdata1$Surface_Area[(newdata1$Surface_Area>500)&(newdata1$Surface_Area<600)]<-"verylow"
newdata1$Surface_Area[(newdata1$Surface_Area>600)&(newdata1$Surface_Area<700)]<-"low"
newdata1$Surface_Area[(newdata1$Surface_Area>700)&(newdata1$Surface_Area<800)]<-"high"
newdata1$Surface_Area[(newdata1$Surface_Area>800)&(newdata1$Surface_Area<900)]<-"veryhigh"

#binning wall area
table(newdata1$Wall_Area)
newdata1$Wall_Area[(newdata1$Wall_Area>200)&(newdata1$Wall_Area<250)]<-"low"
newdata1$Wall_Area[(newdata1$Wall_Area>250)&(newdata1$Wall_Area<350)]<-"medium"
newdata1$Wall_Area[(newdata1$Wall_Area>350)&(newdata1$Wall_Area<450)]<-"high"

#binning roofarea
table(newdata1$Roof_Area)
newdata1$Roof_Area[(newdata1$Roof_Area==110.25)]<-"verylow"
newdata1$Roof_Area[(newdata1$Roof_Area==122.5)]<-"low"
newdata1$Roof_Area[(newdata1$Roof_Area==147)]<-"high"
newdata1$Roof_Area[(newdata1$Roof_Area==220.5)]<-"veryhigh"

#binning overall height
table(newdata1$Overall_Height)
newdata1$Overall_Height[(newdata1$Overall_Height)==3.5]<-'less'
newdata1$Overall_Height[(newdata1$Overall_Height)==7]<-'more'

#binning orientation
table(newdata1$Orientation)
newdata1$Orientation[(newdata1$Orientation>=2)&(newdata1$Orientation<3)]<-"south"
newdata1$Orientation[(newdata1$Orientation>=3)&(newdata1$Orientation<4)]<-"north"
newdata1$Orientation[(newdata1$Orientation>=4)&(newdata1$Orientation<5)]<-"east"
newdata1$Orientation[(newdata1$Orientation>=5)&(newdata1$Orientation<6)]<-"west"

#binning glazing area
table(newdata1$Glazing_Area)
newdata1$Glazing_Area[(newdata1$Glazing_Area)==0]<-"noglass"
newdata1$Glazing_Area[(newdata1$Glazing_Area>=0.1)&(newdata1$Glazing_Area<0.25)]<-"low"
newdata1$Glazing_Area[(newdata1$Glazing_Area)==0.25]<-"medium"
newdata1$Glazing_Area[(newdata1$Glazing_Area)==0.4]<-"high"

#binning glazing area distribution
table(newdata1$Glazing_Area_Distribution)
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution)==0]<-"noglass"
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution)==1]<-"verylow"
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution>=2)&(newdata1$Glazing_Area_Distribution<3)]<-"low"
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution>=3)&(newdata1$Glazing_Area_Distribution<4)]<-"medium"
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution)==4]<-"high"
newdata1$Glazing_Area_Distribution[(newdata1$Glazing_Area_Distribution)==5]<-"veryhigh"

#converting all variables into factors
str(newdata1)
newdata1<-lapply(newdata1, factor)
newdata1<-as.data.frame(newdata1)
#splitting data into train and test sets
library(caret)
set.seed(1234)
split=createDataPartition(newdata1$Heating_Load,p=0.80,list = F)
traindata=newdata1[split,]
testdata=newdata1[-split,]
#naive bayes 
library(caret)
library(e1071)
nb_model<-naiveBayes(Heating_Load~.,data = traindata)
names(testdata)
pred<-predict(nb_model,testdata[,-9])
df<-as.data.frame(cbind(pred,testdata$Heating_Load))
tab<-confusionMatrix(testdata$Heating_Load,pred)
tab$overall


#binning knnimputed data
Heating_Load<-discretize(newdata2$Heating_Load,"equalfreq",5)
Cooling_Load<-discretize(newdata2$Cooling_Load,"equalfreq",5)
newdata2$Heating_Load<-as.factor(Heating_Load$X)
newdata2$Cooling_Load<-as.factor(Cooling_Load$X)

levels(newdata2$Cooling_Load)
levels(newdata2$Heating_Load)

#renaming class labels
levels(newdata2$Heating_Load)[1]="verylow"
levels(newdata2$Heating_Load)[2]="low"
levels(newdata2$Heating_Load)[3]="medium"
levels(newdata2$Heating_Load)[4]="high"
levels(newdata2$Heating_Load)[5]="veryhigh"


levels(newdata2$Cooling_Load)[1]="verylow"
levels(newdata2$Cooling_Load)[2]="low"
levels(newdata2$Cooling_Load)[3]="medium"
levels(newdata2$Cooling_Load)[4]="high"
levels(newdata2$Cooling_Load)[5]="veryhigh"

#binning relativecompactness
table(newdata2$Relative_Compactness)
newdata2$Relative_Compactness[(newdata2$Relative_Compactness>0)&(newdata2$Relative_Compactness<=0.7)]<-"verylow"
newdata2$Relative_Compactness[(newdata2$Relative_Compactness>0.7)&(newdata2$Relative_Compactness<=0.8)]<-"low"
newdata2$Relative_Compactness[(newdata2$Relative_Compactness>0.8)&(newdata2$Relative_Compactness<0.9)]<-"high"
newdata2$Relative_Compactness[(newdata2$Relative_Compactness>=0.9)&(newdata2$Relative_Compactness<=1)]<-"veryhigh"

#binning surface area
table(newdata2$Surface_Area)
newdata2$Surface_Area[(newdata2$Surface_Area>500)&(newdata2$Surface_Area<600)]<-"verylow"
newdata2$Surface_Area[(newdata2$Surface_Area>600)&(newdata2$Surface_Area<700)]<-"low"
newdata2$Surface_Area[(newdata2$Surface_Area>700)&(newdata2$Surface_Area<800)]<-"high"
newdata2$Surface_Area[(newdata2$Surface_Area>800)&(newdata2$Surface_Area<900)]<-"veryhigh"

#binning wall area
table(newdata2$Wall_Area)
newdata2$Wall_Area[(newdata2$Wall_Area>200)&(newdata2$Wall_Area<250)]<-"low"
newdata2$Wall_Area[(newdata2$Wall_Area>250)&(newdata2$Wall_Area<350)]<-"medium"
newdata2$Wall_Area[(newdata2$Wall_Area>350)&(newdata2$Wall_Area<450)]<-"high"

#binning roofarea
table(newdata2$Roof_Area)
newdata2$Roof_Area[(newdata2$Roof_Area<=115)]<-"verylow"
newdata2$Roof_Area[(newdata2$Roof_Area==122.5)]<-"low"
newdata2$Roof_Area[(newdata2$Roof_Area==147)]<-"high"
newdata2$Roof_Area[(newdata2$Roof_Area==220.5)]<-"veryhigh"

#binning overall height
table(newdata2$Overall_Height)
newdata2$Overall_Height[(newdata2$Overall_Height)==3.5]<-'less'
newdata2$Overall_Height[(newdata2$Overall_Height)==7]<-'more'

#binning orientation
table(newdata2$Orientation)
newdata2$Orientation[(newdata2$Orientation>=2)&(newdata2$Orientation<3)]<-"south"
newdata2$Orientation[(newdata2$Orientation>=3)&(newdata2$Orientation<4)]<-"north"
newdata2$Orientation[(newdata2$Orientation>=4)&(newdata2$Orientation<5)]<-"east"
newdata2$Orientation[(newdata2$Orientation>=5)&(newdata2$Orientation<6)]<-"west"

#binning glazing area
table(newdata2$Glazing_Area)
newdata2$Glazing_Area[(newdata2$Glazing_Area)==0]<-"noglass"
newdata2$Glazing_Area[(newdata2$Glazing_Area>=0.1)&(newdata2$Glazing_Area<0.25)]<-"low"
newdata2$Glazing_Area[(newdata2$Glazing_Area)==0.25]<-"medium"
newdata2$Glazing_Area[(newdata2$Glazing_Area)==0.4]<-"high"

#binning glazing area distribution
table(newdata2$Glazing_Area_Distribution)
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution)==0]<-"noglass"
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution)==1]<-"verylow"
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution>=2)&(newdata2$Glazing_Area_Distribution<3)]<-"low"
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution>=3)&(newdata2$Glazing_Area_Distribution<4)]<-"medium"
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution)==4]<-"high"
newdata2$Glazing_Area_Distribution[(newdata2$Glazing_Area_Distribution)==5]<-"veryhigh"

library(caret)
set.seed(1234)
split=createDataPartition(newdata2$Heating_Load,p=0.80,list = F)
traindata1=newdata2[split,]
testdata1=newdata2[-split,]
#naive bayes 
library(e1071)
nb_model<-naiveBayes(Heating_Load~.,data = traindata1)
names(testdata1)
pred1<-predict(nb_model,testdata1[,-9])
df<-as.data.frame(cbind(pred1,testdata1$Heating_Load))
tab1<-confusionMatrix(testdata1$Heating_Load,pred1)

tab1$overall#acc=71.7(with knnimputation)
tab$overall#acc=72.3(with centralimputation)



