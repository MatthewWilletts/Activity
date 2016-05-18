## Random Forest analysis for 10 prototype participants


library(randomForest)
library(data.table)
library(TLBC)
library("upclass")
library(extraTrees)

#Window size in seconds
ws=60

#frequency of data
rate=50

if(Sys.info()[['sysname']]=="Darwin"){
  cleanDataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/clean_data'
  cleanBoutDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/clean_data'
  dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data'
  
  
  #Linux
} else if(Sys.info()[['sysname']]=='Linux'){
  cleanDataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/clean_data'
  cleanBoutDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/clean_data'
  dataDirectory<-'/data/rockptarmigan/willetts/Prototype_data'
  
  
}

#Temporary directory
tempDirectory<-paste0(dataDirectory,'/temp')

#Directories for RF and HMM models
RFoutput<-paste0(tempDirectory,'/RFoutput')
HMMoutput<-paste0(tempDirectory,'/HMMoutput')
Predictions<-paste0(tempDirectory,'/Predictions')

outputLabelDirectory<-paste0(tempDirectory,'/Bout_Labels_',ws)
outputFeatureDirectory<-paste0(tempDirectory,'/Accel_Features_',ws)

#Semi Supervised Learning Output
semiSupervisedLabelDirectory<-paste0(dataDirectory,'/SemiSupLabels')
semiSupervisedFeatureDirectory<-paste0(dataDirectory,'/SemiSupFeatures')


#Cut Down to certainly correct output
certainlyTrueLabelDirectory<-paste0(dataDirectory,'/CertainlyTrueLabels')
certainlyTrueFeatureDirectory<-paste0(dataDirectory,'/CertainlyTrueFeatures')



listOfIndividuals<-list.files(cleanBoutDirectory)
listOfDataFiles<-list.files(cleanDataDirectory)

identifiers<-gsub(listOfIndividuals,pattern = '.csv',replacement = '')



#Test accuracy when training RF on 9 individuals, testing on 1

for(i in 1:length(identifiers)){

model_loc_ss<-paste0(tempDirectory,'/testOn',identifiers[i],'_ss_Model.RData')

#annotationsToLabels(annotations =trainingBoutDirectory,winSize = 60,names = identifiers[i] )

trainModel(annotations = semiSupervisedLabelDirectory,accelerometers = semiSupervisedFeatureDirectory,GPS = NULL,winSize = ws,modelName =model_loc_ss,names =identifiers[-i],ntree = 2000 )

model_loc_ct<-paste0(tempDirectory,'/testOn',identifiers[i],'_ct_Model.RData')
trainModel(annotations = certainlyTrueLabelDirectory,accelerometers = certainlyTrueFeatureDirectory,GPS = NULL,winSize = ws,modelName =model_loc_ct,names =identifiers[-i] ,ntree=2000)


}


performance_CT<-list()
performance_SS<-list()

for(i in 1:length(identifiers)){
  
  
  model_loc_ss<-paste0(tempDirectory,'/testOn',identifiers[i],'_ss_Model.RData')
  classify(accelerometers = semiSupervisedFeatureDirectory,modelName =model_loc_ss,saveDir = file.path(Predictions,'SS'),names= identifiers[i] )
  
  performance_SS[[i]]<-calcPerformance(annotations = semiSupervisedLabelDirectory,predictions = file.path(Predictions,'SS'),winSize = ws,names = identifiers[i])
  
  #annotationsToLabels(annotations =trainingBoutDirectory,winSize = 60,names = identifiers[i] )
  

  model_loc_ct<-paste0(tempDirectory,'/testOn',identifiers[i],'_ct_Model.RData')
  classify(accelerometers = certainlyTrueFeatureDirectory,modelName =model_loc_ct,saveDir = file.path(Predictions,'CT'),names= identifiers[i] )
  performance_CT[[i]]<-calcPerformance(annotations = certainlyTrueLabelDirectory,predictions = file.path(Predictions,'CT'),winSize = ws,names = identifiers[i])
  
}

for (i in 1:length(identifiers)){
  print(identifiers[i])
  print(performance_CT[[i]]$positive)

}

unlist(lapply(X = performance_CT,FUN = function(x) sum(x$overall[1])))
