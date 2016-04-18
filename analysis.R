## Random Forest analysis for 10 prototype participants

library(randomForest)
library(data.table)
library(TLBC)

dataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/'
labelDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/'

testdataDirectory<-'/data/rockptarmigan/willetts/testaccel/'
testlabelDirectory<-'/data/rockptarmigan/willetts/testannot/p005'

trainModel(annotations = testlabelDirectory,accelerometers = testdataDirectory,GPS = NULL,winSize = 60)


accelFiles<-list.files(dataDirectory)
accelFiles<-accelFiles[substr(accelFiles, 1, 1)=='p']
labelFiles<-list.files(labelDirectory)
labelFiles<-labelFiles[substr(labelFiles, 1, 3)=='p']

data<-fread(input =paste0(dataDirectory, accelFiles[1]))

