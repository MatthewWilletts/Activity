## further data cleaning and SemiSupervised filling in of 10 initial data points

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


#Temp data directories
trainingAccelDirectory<-paste0(tempDirectory,'/AccelTraining')
testingAccelDirectory<-paste0(tempDirectory,'/AccelTesting')

trainingBoutDirectory<-paste0(tempDirectory,'/BoutTraining')
testingBoutDirectory<-paste0(tempDirectory,'/BoutTesting')

trainingFeatureDirectory<-paste0(trainingAccelDirectory,'_Features_',ws)
trainingLabelDirectory<-paste0(trainingBoutDirectory,'_Labels_',ws)

testingFeatureDirectory<-paste0(testingAccelDirectory,'_Features_',ws)
testingLabelDirectory<-paste0(testingBoutDirectory,'_Labels_',ws)

outputLabelDirectory<-paste0(tempDirectory,'/Bout_Labels_',ws)
outputFeatureDirectory<-paste0(tempDirectory,'/Accel_Features_',ws)

#Semi Supervised Learning Output
semiSupervisedLabelDirectory<-paste0(dataDirectory,'/SemiSupLabels')
semiSupervisedFeatureDirectory<-paste0(dataDirectory,'/SemiSupFeatures')


#Cut Down to certainly correct output
certainlyTrueLabelDirectory<-paste0(dataDirectory,'/CertainlyTrueLabels')
certainlyTrueFeatureDirectory<-paste0(dataDirectory,'/CertainlyTrueFeatures')


## Train on first half of data for each individual, test on the second

listOfIndividuals<-list.files(cleanBoutDirectory)
listOfDataFiles<-list.files(cleanDataDirectory)

identifiers<-gsub(listOfIndividuals,pattern = '.csv',replacement = '')

InstanceData<-list()
FeatureData<-list()

IndexOfInstanceFiles<-list()

performance<-list()

trainingNoLabel<-list()
testingNoLabel<-list()


for (i in 1:length(listOfIndividuals)){
  #create instance level information
  boutFileAddress<-paste(cleanBoutDirectory,listOfIndividuals[i],sep='/')
  if(file.exists(file.path(outputLabelDirectory,identifiers[i]))==FALSE){
  extractLabelsSingleFile(inputFile = boutFileAddress,outputDir = outputLabelDirectory,winSize = ws)
  }
  #create features
  accelFileAddress<-paste(cleanDataDirectory,listOfDataFiles[i],sep='/')
  if(file.exists(file.path(outputFeatureDirectory,identifiers[i]))==FALSE){
  extractAccFeatsFile(inputFile = accelFileAddress,outputPath = file.path(outputFeatureDirectory,identifiers[i]),winSize = 60)
  }
}

  


for (i in 1:length(listOfIndividuals)){

  #Step 1 - use semi supervised learning to fill in the blank behavior spaces
  
  #Load in instance level labels
  InstanceData[[i]]<-NULL
  InstanceDir<-file.path(outputLabelDirectory,identifiers[i])
  InstanceFiles<-list.files(InstanceDir)
  tempInstanceData<-NULL
  
  #Load in features
  FeatureData[[i]]<-NULL
  tempFeatureData<-NULL
  FeatureDir<-file.path(outputFeatureDirectory,identifiers[i])
  FeatureFiles<-list.files(FeatureDir)
  
  
  
  for(k in 1:length(InstanceFiles)){
    
  #load in instance data
  temptempInstanceData<-read.csv(file=file.path(InstanceDir,InstanceFiles[k]),stringsAsFactors = F)
  
  #load in feature data
  temptempFeatureData<-read.csv(file=file.path(FeatureDir,FeatureFiles[k]),stringsAsFactors = F)
  
  #discard all data before first labelled data point and after last labelled point
  kx<-which(!temptempInstanceData$behavior=='nolabel')
  if(length(kx)>0){
  maxIndex<-min(kx[length(kx)],nrow(temptempFeatureData))
  temptempInstanceData<-temptempInstanceData[kx[1]:maxIndex,]
  tempInstanceData<-rbind(tempInstanceData,temptempInstanceData)

  temptempFeatureData<-temptempFeatureData[kx[1]:maxIndex,]
  tempFeatureData<-rbind(tempFeatureData,temptempFeatureData)
  }
  }
  InstanceData[[i]]<-tempInstanceData
  rm(tempInstanceData)
  rm(temptempInstanceData)
  FeatureData[[i]]<-tempFeatureData
  rm(tempFeatureData)
  rm(temptempFeatureData)
  
}


#cut down Instance data to size of Feature data, or vice versa
for (i in 1:length(listOfIndividuals)){

  if(nrow(FeatureData[[i]])<nrow(InstanceData[[i]])){
    InstanceData[[i]]<-InstanceData[[i]][seq(1,nrow(FeatureData[[i]])),]
  } else {
    FeatureData[[i]]<-FeatureData[[i]][seq(1,nrow(InstanceData[[i]])),]
  }
}

#add participant labels to data
  
for (i in 1:length(listOfIndividuals)){
      InstanceData[[i]]$name<-identifiers[i]
      FeatureData[[i]]$name<-identifiers[i]
    
  }

for (i in 1:length(listOfIndividuals)){
  print(nrow(InstanceData[[i]]))
  print(nrow(FeatureData[[i]]))
}


AllFeatureData<-do.call("rbind", FeatureData)
AllInstanceData<-do.call("rbind", InstanceData)

#reduce the number of different labels

AllInstanceData<-reduceLabels(data=AllInstanceData,labelsToReduce=list(c('gardening','standing'),c('in-vehicle')),overallLabel =c('sitting','driving'))


  #which instances have missing labels
  ix<-which(AllInstanceData$behavior=='nolabel')
  unlabelledInstance<-AllInstanceData[ix,]
  
  labelledInstance<-as.factor(as.vector(AllInstanceData[-ix,2]))
  labelCode<-levels(labelledInstance)
  levels(labelledInstance)<-1:length(levels(labelledInstance))
  labelledInstance<-as.numeric(labelledInstance)
  
  labelledFeature<-AllFeatureData[-ix,]
  labelledFeature$timestamp<-NULL
  labelledFeature<-as.matrix(labelledFeature)
  
  unlabelledFeature<-AllFeatureData[ix,]
  unlabelledFeature$timestamp<-NULL
  unlabelledFeature<-as.matrix(unlabelledFeature)
    
  semiSupOutputComplete<-upclassify(Xtrain = labelledFeature,cltrain = labelledInstance,Xtest = unlabelledFeature)
  
  newLabels<-semiSupOutputComplete$Best$test$cl
  
    
  newLabels<-as.factor(newLabels)
  levels(newLabels)<-labelCode
  newLabels<-as.character(newLabels)
  
  OutputInstanceData<-AllInstanceData
  OutputInstanceData$behavior[ix]=newLabels
  
  
  semiSupCounter<-1
  
  
#Now Output semisupervise labels and features
  #and also certainly true 
for (i in 1:length(listOfIndividuals)){

  #Load in instance level labels
  InstanceDir<-file.path(outputLabelDirectory,identifiers[i])
  InstanceFiles<-list.files(InstanceDir)
  FeatureDir<-file.path(outputFeatureDirectory,identifiers[i])
  FeatureFiles<-list.files(FeatureDir)
  semiSupervisedLabelOutputDir<-file.path(semiSupervisedLabelDirectory,identifiers[i])
  semiSupervisedFeatureOutputDir<-file.path(semiSupervisedFeatureDirectory,identifiers[i])
  
    outputrow<-nrow(InstanceData[[i]])
    labelOutputData<-OutputInstanceData[seq(semiSupCounter,semiSupCounter+outputrow-1),]
    featureOutputData<-AllFeatureData[seq(semiSupCounter,semiSupCounter+outputrow-1),]
    
    semiSupCounter<-semiSupCounter+outputrow
    
    dir.create(semiSupervisedLabelOutputDir)
    dir.create(semiSupervisedFeatureOutputDir)
    
               
    if(file.exists(paste0(semiSupervisedLabelOutputDir,'/',InstanceFiles[1]))==FALSE){
      write.csv(x=labelOutputData,file=paste0(semiSupervisedLabelOutputDir,'/',InstanceFiles[1]),row.names=FALSE,append = FALSE)
    }
    if(file.exists(paste0(semiSupervisedFeatureOutputDir,'/',FeatureFiles[1]))==FALSE){
      write.csv(x=featureOutputData,file=paste0(semiSupervisedFeatureOutputDir,'/',FeatureFiles[1]),row.names=FALSE,append = FALSE)
    }
    
    certainlyTrueLabelOutputDir<-file.path(certainlyTrueLabelDirectory,identifiers[i])
    certainlyTrueFeatureOutputDir<-file.path(certainlyTrueFeatureDirectory,identifiers[i])
    
    ix<-which(!InstanceData[[i]]$behavior=='nolabel')
    certainlyTrueLabels<-InstanceData[[i]][ix,]
    certainlyTrueFeatures<-FeatureData[[i]][ix,]
    
    dir.create(certainlyTrueLabelOutputDir)
    dir.create(certainlyTrueFeatureOutputDir)
    
    
    if(file.exists(paste0(certainlyTrueLabelOutputDir,'/',InstanceFiles[1]))==FALSE){
      write.csv(x=certainlyTrueLabels,file=paste0(certainlyTrueLabelOutputDir,'/',InstanceFiles[1]),row.names=FALSE,append = FALSE)
    }
    if(file.exists(paste0(certainlyTrueFeatureOutputDir,'/',FeatureFiles[1]))==FALSE){
      write.csv(x=certainlyTrueFeatures,file=paste0(certainlyTrueFeatureOutputDir,'/',FeatureFiles[1]),row.names=FALSE,append = FALSE)
    }
  }