## Random Forest analysis for 10 prototype participants

library(randomForest)
library(data.table)
library(TLBC)

#Window size in seconds
ws=60

if(Sys.info()[['sysname']]=="Darwin"){
  cleanDataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/clean_data'
  cleanBoutDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/clean_data'
  dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data'
  
  outputLabelDirectory<-paste0('/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/clean_data_Labels_',ws)
  outputFeatureDirectory<-paste0('/Users/Matthew/Documents/Oxford/Activity/Prototype_data/clean_data_Features_',ws)
  
  #Linux
} else if(Sys.info()[['sysname']]=='Linux'){
  cleanDataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/clean_data'
  cleanBoutDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/clean_data'
  dataDirectory<-'/data/rockptarmigan/willetts/Prototype_data'
  
  outputLabelDirectory<-paste0('/data/rockptarmigan/willetts/Prototype_data/all_participants/clean_data_Labels_',ws)
  outputFeatureDirectory<-paste0('/data/rockptarmigan/willetts/Prototype_data/clean_data_Features_',ws)
}

testRFoutput<-paste0(dataDirectory,'/testRF')
testHMMoutput<-paste0(dataDirectory,'/testHMM')

#model location
model_loc<-paste0(dataDirectory,'/myModel.RData')


trainModel(annotations = cleanBoutDirectory,accelerometers = cleanDataDirectory,GPS = NULL,winSize = ws,modelName =model_loc )


testRF(featDirs = outputFeatureDirectory,modelName = model_loc,saveDir = testRFoutput,testNames = 'p005')
testHMM(predDir=testRFoutput, modelName=model_loc, saveDir =testHMMoutput , names='p005')

