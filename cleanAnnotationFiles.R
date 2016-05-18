## Clean annotation data for Forest analysis for 10 prototype participants

rm(list = ls())


library(randomForest)
library(data.table)
library(stringr)

#Laptop
if(Sys.info()[['sysname']]=="Darwin"){
dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/'
labelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/'
cleanDataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/clean_data/'
cleanLabelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/clean_data/'
functionDirectory<-'/Users/Matthew/Documents/Oxford/Activity/'
#Linux
} else if(Sys.info()[['sysname']]=='Linux'){
dataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/'
labelDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/'
cleanDataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/clean_data/'
cleanLabelDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/clean_data/'
functionDirectory<-'~/Documents/Activity/'
}

source(paste0(functionDirectory,'functions.R'))

rawAccelDataDir<-paste0(dataDirectory,'rawAccelData/')

accelFiles<-list.files(rawAccelDataDir)
accelFiles<-accelFiles[substr(accelFiles, 1, 1)=='p']
labelFiles<-list.files(labelDirectory)
labelFiles<-labelFiles[substr(labelFiles, 1, 3)=='csv']

temp<-sapply(labelFiles, FUN=function(x) {strsplit(x, split='[.]')[[1]][1]})
temp<-as.numeric(sub('csvP','',temp))
ix<-order(temp)
labelFiles<-labelFiles[ix]
keeps<-c('participant','NewStart','NewEnd','label')

rate=50

FirstLast<-list()

for(i in 1:length(labelFiles)){

FirstLast[[i]]<-cleanLabelData(labelDirectory,labelFile=labelFiles[i],cleanLabelDirectory)

}

FirstLast<-do.call(rbind,FirstLast)

colnames(FirstLast)<-c('identifier','First','Last')
FirstLast$Duration<-FirstLast$Last-FirstLast$First
  

#Create complete training chunks of accelerometer data
cleanAccelData(FirstLast=FirstLast[9:10,],accelFiles=accelFiles[9:10],rawAccelDataDir = rawAccelDataDir,cleanDataDirectory=cleanDataDirectory)










