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
sum(is.na(newdata))
#missing value plot
library(Amelia)
missmap(newdata)
names(newdata)

#imputing missing values
library(DMwR)
newdata1<-centralImputation(newdata)
newdata2<-knnImputation(newdata,k=3)

#correlation plot
library(corrplot)
corr=cor(newdata1)
corrplot(corr,method = "number")

#from the above correlation plot we can infer that both the target variables are positively correlated by 0.98 
#and also the correlation of both the response variables with each of the independen variables is almost same.
#hence we can remove one of the response variables as predicting any one of them would make better sense than predicting both
names(newdata1)
newdata1=newdata1[,c(-10)]
newdata2=newdata2[,-c(10)]
names(newdata2)
#binning centralimputed data
#binning response variables
library(infotheo)
hist(newdata1$Heating_Load)

Heating_Load<-discretize(newdata1$Heating_Load,"equalfreq",5)
newdata1$Heating_Load<-as.factor(Heating_Load$X)
levels(newdata1$Heating_Load)

#renaming class labels
levels(newdata1$Heating_Load)[1]="verylow"
levels(newdata1$Heating_Load)[2]="low"
levels(newdata1$Heating_Load)[3]="medium"
levels(newdata1$Heating_Load)[4]="high"
levels(newdata1$Heating_Load)[5]="veryhigh"

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
names(traindata)

ggplot(traindata, aes(Relative_Compactness, fill = Heating_Load)) + geom_bar()+
  labs(title = "relative compactness vs heating load", x = "Relative_Compactness", y = "Heating_load")

ggplot(traindata, aes(Surface_Area, fill = Heating_Load)) + geom_bar()+
  labs(title = "Surface area vs heating load", x = "Surface_Area", y = "Heating_load")

ggplot(traindata, aes(Wall_Area, fill = Heating_Load)) + geom_bar()+
  labs(title = "wall area vs heating load", x = "Wall_Area", y = "Heating_load")

ggplot(traindata, aes(Roof_Area, fill = Heating_Load)) + geom_bar()+
  labs(title = "Roof area vs heating load", x = "Roof_Area", y = "Heating_load")

ggplot(traindata, aes(Overall_Height, fill = Heating_Load)) + geom_bar()+
  labs(title = "overall height vs heating load", x = "Overall_Height", y = "Heating_load")

ggplot(traindata, aes(Orientation, fill = Heating_Load)) + geom_bar()+
  labs(title = "orientation vs heating load", x = "Orientation", y = "Heating_load")

ggplot(traindata, aes(Glazing_Area, fill = Heating_Load)) + geom_bar()+
  labs(title = "glazing area vs heating load", x = "Glazing_Area", y = "Heating_load")

ggplot(traindata, aes(Glazing_Area_Distribution, fill = Heating_Load)) + geom_bar()+
  labs(title = "Glazing area distribution vs heating load", x = "Glazing_Area_Distribution", y = "Heating_load")

#model buliding
#naive bayes 
library(caret)
library(e1071)
nb_model<-naiveBayes(Heating_Load~.,data = traindata)
pred<-predict(nb_model,testdata[,-9])
tabnaive=confusionMatrix(testdata$Heating_Load,pred)
library(MLmetrics)
F1_Score(testdata$Heating_Load,pred,positive = NULL)
Accuracy(testdata$Heating_Load,pred)
Recall(testdata$Heating_Load,pred)
Precision(testdata$Heating_Load,pred)

#logistic regression
library(caret)
ctrl=trainControl(method="cv")
modellog=train(Heating_Load ~ ., data=newdata1, method="multinom", metric="Accuracy",trControl=ctrl, subset=split)
predlogistic<-predict(modellog,newdata = testdata)
F1_Score(testdata$Heating_Load,predlogistic)
Accuracy(testdata$Heating_Load,predlogistic)
Recall(testdata$Heating_Load,predlogistic)
Precision(testdata$Heating_Load,predlogistic)

#randomforest
modelrf= train(Heating_Load~ ., data=newdata1, method="rf", metric="Accuracy",trControl=ctrl, subset=split)
predrf<-predict(modelrf,newdata = testdata)
F1_Score(testdata$Heating_Load,predrf)
Accuracy(testdata$Heating_Load,predrf)
Recall(testdata$Heating_Load,predrf)
Precision(testdata$Heating_Load,predrf)

#rpart
modelrpart=train(Heating_Load~., data=newdata1, method="rpart", metric="Accuracy",trControl=ctrl, subset=split)
predrpart<-predict(modelrpart,newdata = testdata)
F1_Score(testdata$Heating_Load,predrpart)
Accuracy(testdata$Heating_Load,predrpart)
Recall(testdata$Heating_Load,predrpart)
Precision(testdata$Heating_Load,predrpart)

#method:2
#treated all the predictors as numeric variables 
rm(list = ls())
library(readxl)
setwd("C:/Users/Admin/Desktop/intern2")
mydataa<-read_xlsx("ENB2012_data.xlsx")
mydataa<-as.data.frame(mydataa)
library(dplyr)
names(mydataa)
newdata=rename(mydataa,Relative_Compactness=X1,
               Surface_Area=X2, 
               Wall_Area=X3,
               Roof_Area=X4,
               Overall_Height=X5, 
               Orientation=X6,
               Glazing_Area=X7, 
               Glazing_Area_Distribution=X8,
               Heating_Load=Y1,
               Cooling_Load=Y2)

names(newdata)

stats<-function(x)
{
   table(x)
}
apply(newdata[,c(-9,-10)],2,stats)

#removing cooling_load
newdataa=newdata[,-c(10)]
names(newdataa)

#binning the target variable heating_load
library(infotheo)
hist(newdataa$Heating_Load)
Heating_Load<-discretize(newdataa$Heating_Load,"equalfreq",5)
newdataa$Heating_Load<-as.factor(Heating_Load$X)
levels(newdataa$Heating_Load)
#renaming class labels
levels(newdataa$Heating_Load)[1]="verylow"
levels(newdataa$Heating_Load)[2]="low"
levels(newdataa$Heating_Load)[3]="medium"
levels(newdataa$Heating_Load)[4]="high"
levels(newdataa$Heating_Load)[5]="veryhigh"

str(newdataa)
#splitting it into train and test datasets
library(caret)
set.seed(1234)
split<-createDataPartition(newdataa$Heating_Load,p=0.80,list=F)
traindataa<-newdataa[split,]
testdataa<-newdataa[-split,]

#since the range of each attribute is different from the others standardize all the predictors
#standardization
library(caret)
preproc=preProcess(traindataa[,-c(9)],method = c("center","scale"))
traindataa[,-c(9)]=predict(preproc,traindataa[,-c(9)])
testdataa[,-c(9)]=predict(preproc,testdataa[,-c(9)])
names(traindataa)
head(traindataa[,-c(9)])

#model building
library(MLmetrics)
#svm
ctrl <- trainControl(method = "repeatedcv",search = "random", repeats = 5,classProbs = T)
radial.svm.tune <- train(x= traindataa[,-9],y=traindataa[,9],
                         method = "svmRadial",
                         metric = "Accuracy", trControl = ctrl)


predictions<-predict(radial.svm.tune,testdataa[,-9])
Accuracy(predictions,testdataa$Heating_Load)#84.868
Recall(predictions,testdataa$Heating_Load)#87.09
Precision(predictions,testdataa$Heating_Load)#87.09
F1_Score(predictions,testdataa$Heating_Load)#87.09

#knn
library(class)
pred1=knn(traindataa[,-c(9)],testdataa[,-c(9)],cl=traindataa$Heating_Load,k=1)
Accuracy(pred1,testdataa$Heating_Load)
Recall(pred1,testdataa$Heating_Load)
Precision(pred1,testdataa$Heating_Load)
F1_Score(pred1,testdataa$Heating_Load)

pred2=knn(traindataa[,-c(9)],testdataa[,-c(9)],cl=traindataa$Heating_Load,k=3)
Accuracy(pred2,testdataa$Heating_Load)
Recall(pred2,testdataa$Heating_Load)
Precision(pred2,testdataa$Heating_Load)
F1_Score(pred2,testdataa$Heating_Load)

pred3=knn(traindataa[,-c(9)],testdataa[,-c(9)],cl=traindataa$Heating_Load,k=5)
Accuracy(pred3,testdataa$Heating_Load)
Recall(pred3,testdataa$Heating_Load)
Precision(pred3,testdataa$Heating_Load)
F1_Score(pred3,testdataa$Heating_Load)

pred4=knn(traindataa[,-c(9)],testdataa[,-c(9)],cl=traindataa$Heating_Load,k=7)
Accuracy(pred4,testdataa$Heating_Load)#83.55
Recall(pred4,testdataa$Heating_Load)#96.55
Precision(pred4,testdataa$Heating_Load)#90.32
F1_Score(pred4,testdataa$Heating_Load)#93.33333

#logistic regression
library(nnet)
set.seed(1234)
split1=createDataPartition(newdataa$Heating_Load,p=0.80,list=F)
ctrl=trainControl(method="Cv")
modellog1=train(Heating_Load ~ ., data=newdataa, method="multinom", metric="Accuracy",trControl=ctrl, subset=split1)
testdata1<-newdataa[-split1,]
predlog<-predict(modellog1,testdata1)

Accuracy(predlog,testdata1$Heating_Load)#79.6
Precision(predlog,testdata1$Heating_Load)#87.09
Recall(predlog,testdata1$Heating_Load)#93.103
F1_Score(predlog,testdata1$Heating_Load)#90.00
names(testdataa)
