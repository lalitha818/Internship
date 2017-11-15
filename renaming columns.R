#loading data set
dataa<-iris

#using tibble library to convert it into dataframe
library(tibble)
dataa<-as_data_frame(dataa)

#using dplyr to rename columns
library(dplyr)
names(dataa)
newdata<-rename(dataa,sl=Sepal.Length,sw=Sepal.Width,pl=Petal.Length,pw=Petal.Width ,ss=Species)
names(newdata)
#renaming columns with dplyr::select()
newdata1<-select(newdata,sll=sl,sww=sw)
names(newdata1)

#using r base functions to rename columns
#get column names
colnames(newdata1)
names(newdata1)[names(newdata1)=="sll"]<-"sl"
names(newdata1)[names(newdata1)=="sww"]<-"sw"
names(newdata1)

#other way
names(dataa)[1]<-'s'
names(dataa)
