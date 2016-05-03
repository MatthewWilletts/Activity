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
  if(file.exists(file.path(outputLabelDirectory,identifiers[i])))==FALSE){
  extractLabelsSingleFile(inputFile = boutFileAddress,outputDir = outputLabelDirectory,winSize = ws)
  }
  #create features
  accelFileAddress<-paste(cleanDataDirectory,listOfDataFiles[i],sep='/')
  extractAccFeatsFile(inputFile = accelFileAddress,outputPath = paste(outputFeatureDirectory,identifiers[i],sep='/'),winSize = 60)
}


for (i in 1:length(listOfIndividuals)){
  
  #Step 1 - use semi supervised learning to fill in the blank behavior spaces
  
  #Load in instance level labels
  InstanceData[[i]]<-NULL
  InstanceDir<-file.path(outputLabelDirectory,identifiers[i])
  InstanceFiles<-list.files(InstanceDir)
  tempInstanceData<-NULL
  for(k in 1:length(InstanceFiles)){
  temptempInstanceData<-read.csv(file=file.path(InstanceDir,InstanceFiles[k]),stringsAsFactors = F)
  tempInstanceData<-rbind(tempInstanceData,temptempInstanceData)
  }
  InstanceData[[i]]<-tempInstanceData
  rm(tempInstanceData)
  rm(temptempInstanceData)
  
  #Load in features
  FeatureData[[i]]<-NULL
  tempFeatureData<-NULL
  FeatureDir<-file.path(outputFeatureDirectory,identifiers[i])
  FeatureFiles<-list.files(FeatureDir)
  for(k in 1:length(FeatureFiles)){
    temptempFeatureData<-read.csv(file=file.path(FeatureDir,FeatureFiles[k]),stringsAsFactors = F)
    tempFeatureData<-rbind(tempFeatureData,temptempFeatureData)
  }
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




AllFeatureData<-do.call("rbind", FeatureData)
AllInstanceData<-do.call("rbind", InstanceData)
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
  
  
  counter<-1
  
for (i in 1:length(listOfIndividuals)){
  for (i in 1:8){
    
  #Load in instance level labels
  InstanceDir<-file.path(outputLabelDirectory,identifiers[i])
  InstanceFiles<-list.files(InstanceDir)
  FeatureDir<-file.path(outputFeatureDirectory,identifiers[i])
  FeatureFiles<-list.files(FeatureDir)
  semiSupervisedOutputDir<-file.path(semiSupervisedLabelDirectory,identifiers[i])
  
  for(k in 1:length(InstanceFiles)){

    tempFeatureData<-read.csv(file=file.path(FeatureDir,FeatureFiles[k]),stringsAsFactors = F)
    outputrow<-nrow(tempFeatureData)
    outputData<-OutputInstanceData[seq(counter,counter+outputrow-1),]
    
    counter<-counter+outputrow
    
    #dir.create(semiSupervisedOutputDir)
               
    #if(file.exists(paste0(semiSupervisedOutputDir,'/',InstanceFiles[k]))==FALSE){
    #  write.csv(x=outputData,file=paste0(semiSupervisedOutputDir,'/',InstanceFiles[k]),row.names=FALSE,append = FALSE)
    #}
    
  }
}
  
  
  
  
  
  
  
  
    
    for(k in 1:length(TempInstanceFiles)){
      print(paste0('file ',k,' out of ',length(TempInstanceFiles))) 
      
      tempInstanceData<-read.csv(file=file.path(InstanceDir,InstanceFiles[k]),stringsAsFactors = F)
      tempFeatureData<-read.csv(file=file.path(FeatureDir,FeatureFiles[k]),stringsAsFactors = F)
        

      iy<-which(tempInstanceData$behavior=='nolabel')
      unlabelledFeature<-tempFeatureData[iy,]
      unlabelledFeature$timestamp<-NULL
      unlabelledFeature<-as.matrix(unlabelledFeature)
  
      semiSupOutput<-upclassify(Xtrain = labelledFeature,cltrain = labelledInstance,Xtest = unlabelledFeature)
  
      newLabels<-semiSupOutput$Best$test$cl
  
      newLabels<-as.factor(newLabels)
      levels(newLabels)<-labelCode
      newLabels<-as.character(newLabels)
  
      tempInstanceData$behavior[iy]=newLabels
    
      #now save newly improved instance data to new directory
      if(file.exists(paste0(semiSupervisedOutputDir,'/',InstanceFiles[k]))==FALSE){
        write.csv(x=tempInstanceData,file=paste0(semiSupervisedOutputDir,'/',InstanceFiles[k]),row.names=FALSE)
      }
    }
  }
  
  
#read in labelled bout information
boutFileAddress<-paste(cleanBoutDirectory,listOfIndividuals[i],sep='/')
CompleteBoutData<-read.csv(file = boutFileAddress,header = T)

#Read in header and data of acceleration file
accelFileAddress<-paste(cleanDataDirectory,listOfDataFiles[i],sep='/')
con <- file(description=accelFileAddress, open="r")
HeaderAccelData<-readLines(con = con, n=10)
close(con)

CompleteAccelData<-read.csv(file = accelFileAddress,skip = 10,header = F)


  
  
  #find which bout cut point is closest to halfway through day
  
  elapsedTime<-cumsum(as.numeric(as.POSIXct(CompleteBoutData$EndDateTime)-as.POSIXct(CompleteBoutData$StartDateTime)))
  q<-which.min(abs(elapsedTime-elapsedTime[length(elapsedTime)]/2))
  
  #when is the cut time
  cutTime<-as.POSIXct(CompleteBoutData$StartDateTime[q+1])
  #how many rows down the accel data is it?
  halfRows<-as.numeric(as.POSIXct(CompleteBoutData$StartDateTime[q+1])-as.POSIXct(CompleteBoutData$StartDateTime[1]))*3600*rate
  
  #create data frames for training and testing data
  trainingBoutData<-CompleteBoutData[seq(1,q),]
  testingBoutData<-CompleteBoutData[-seq(1,q),]
  
  trainingAccelData<-CompleteAccelData[seq(1,halfRows),]
  testingAccelData<-CompleteAccelData[-seq(1,halfRows),]
  
  #Remove the data points with no 
  
  trainingHeaderAccelData<-HeaderAccelData
  testingHeaderAccelData<-HeaderAccelData
  testingHeaderAccelData[3]<-paste0("Start Time ",substr(as.character(cutTime), nchar(as.character(cutTime))-7, nchar(as.character(cutTime))))

  #Save to directories
  
  #Training Accel Data
  if(file.exists(paste0(trainingAccelDirectory,'/',trainingBoutData$identifier[1],'.csv'))==FALSE){
    write(x=trainingHeaderAccelData,file=paste0(trainingAccelDirectory,'/',trainingBoutData$identifier[1],'.csv'))
    write.table(x=trainingAccelData,file=paste0(trainingAccelDirectory,'/',trainingBoutData$identifier[1],'.csv'),append = TRUE,row.names = FALSE,col.names = FALSE,sep=',') 
  }
  
  #Testing Accel Data
  if(file.exists(paste0(testingAccelDirectory,'/',testingBoutData$identifier[1],'.csv'))==FALSE){
    write(x=testingHeaderAccelData,file=paste0(testingAccelDirectory,'/',testingBoutData$identifier[1],'.csv'))
    write.table(x=testingAccelData,file=paste0(testingAccelDirectory,'/',testingBoutData$identifier[1],'.csv'),append = TRUE,row.names = FALSE,col.names = FALSE,sep=',') 
  }
  
  #Training Bout Data
  if(file.exists(paste0(trainingBoutDirectory,'/',trainingBoutData$identifier[1],'.csv'))==FALSE){
    write.csv(x=trainingBoutData,file=paste0(trainingBoutDirectory,'/',trainingBoutData$identifier[1],'.csv'),row.names=FALSE)
  }
  
  #Testing Bout Data
  if(file.exists(paste0(testingBoutDirectory,'/',testingBoutData$identifier[1],'.csv'))==FALSE){
    write.csv(x=testingBoutData,file=paste0(testingBoutDirectory,'/',testingBoutData$identifier[1],'.csv'),row.names=FALSE)
  }
  

  model_loc<-paste0(tempDirectory,'/',identifiers[i],'_Model.RData')
  
  
  
  #Now train model on training data
  
  #annotationsToLabels(annotations =trainingBoutDirectory,winSize = 60,names = identifiers[i] )
  
  trainModel(annotations = paste0(trainingBoutDirectory,'/',identifiers[i],'.csv'),accelerometers = trainingAccelDirectory,GPS = NULL,winSize = ws,modelName =model_loc,names =identifiers[i] )
  
  
  
  
  #Now apply RF and HMM model to testing data
  classify(accelerometers = testingAccelDirectory,modelName =model_loc,saveDir = Predictions,names= identifiers[i] )
  
  #Now calculate performance
  performance[[i]]<-calcPerformance(annotations = paste0(testingBoutDirectory,'/',identifiers[i],'.csv'),predictions = Predictions,winSize = ws,names = identifiers[i])
  
  
  
}


trueLabelFiles<-list.files(paste0(testingLabelDirectory,'/',identifiers[i]))
trueLabels<-read.csv(paste0(testingLabelDirectory,'/',identifiers[i],'/',trueLabelFiles[1]))



testRF(featDirs = outputFeatureDirectory,modelName = model_loc,saveDir = RFoutput,testNames = identifiers[i])





#model location
model_loc<-paste0(dataDirectory,'/myModel.RData')


trainModel(annotations = cleanBoutDirectory,accelerometers = cleanDataDirectory,GPS = NULL,winSize = ws,modelName =model_loc,names =  )


testRF(featDirs = outputFeatureDirectory,modelName = model_loc,saveDir = testRFoutput,testNames = 'p005')
testHMM(predDir=testRFoutput, modelName=model_loc, saveDir =testHMMoutput , names='p005')

