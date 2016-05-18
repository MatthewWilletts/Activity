#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

library(optparse)
library(randomForest)
library(data.table)
library(RSpectra)
library(mhsmm)
library(parallel)

#First, define data directories

dataDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/capture-processed'
labelDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/label-data/label-dictionary-9-classes'

#Directories for RF and HMM models
RFoutput<-paste0(dataDirectory,'/RFoutput')
HMMoutput<-paste0(dataDirectory,'/HMMoutput')

#Directories for predictions
Predictions<-paste0(dataDirectory,'/Predictions')


listOfFeatureFiles<-list.files(dataDirectory,pattern = "\\Epoch[.]csv$")
listOfFFTFiles<-list.files(dataDirectory,pattern = "\\Epoch_fft[.]csv$")

listOfLabelFiles<-list.files(labelDirectory,pattern = "\\[.]csv$")


identifiers<-gsub(listOfIndividuals,pattern = "\\Epocj.csv$",replacement = '')

cat(head(identifiers))


InstanceData<-list()
FeatureData<-list()

IndexOfInstanceFiles<-list()

performance<-list()

trainingNoLabel<-list()
testingNoLabel<-list()
# 
# #Load up Data
# for (i in 1:length(listOfIndividuals)){
#   #create instance level information
#   boutFileAddress<-paste(cleanBoutDirectory,listOfIndividuals[i],sep='/')
#   if(file.exists(file.path(outputLabelDirectory,identifiers[i]))==FALSE){
#     extractLabelsSingleFile(inputFile = boutFileAddress,outputDir = outputLabelDirectory,winSize = ws)
#   }
#   #create features
#   accelFileAddress<-paste(cleanDataDirectory,listOfDataFiles[i],sep='/')
#   if(file.exists(file.path(outputFeatureDirectory,identifiers[i]))==FALSE){
#     extractAccFeatsFile(inputFile = accelFileAddress,outputPath = file.path(outputFeatureDirectory,identifiers[i]),winSize = 60)
#   }
#   
#   
#   #Step 1 - Load up all data
#   
#   #Load in instance level labels
#   InstanceData[[i]]<-NULL
#   InstanceDir<-file.path(outputLabelDirectory,identifiers[i])
#   InstanceFiles<-list.files(InstanceDir)
#   tempInstanceData<-NULL
#   
#   #Load in features
#   FeatureData[[i]]<-NULL
#   tempFeatureData<-NULL
#   FeatureDir<-file.path(outputFeatureDirectory,identifiers[i])
#   FeatureFiles<-list.files(FeatureDir)
#   
#   
#   
#   for(k in 1:length(InstanceFiles)){
#     
#     #load in instance data
#     temptempInstanceData<-read.csv(file=file.path(InstanceDir,InstanceFiles[k]),stringsAsFactors = F)
#     
#     #load in feature data
#     temptempFeatureData<-read.csv(file=file.path(FeatureDir,FeatureFiles[k]),stringsAsFactors = F)
#     
#     #discard all data before first labelled data point and after last labelled point
#     kx<-which(!temptempInstanceData$behavior=='nolabel')
#     if(length(kx)>0){
#       maxIndex<-min(kx[length(kx)],nrow(temptempFeatureData))
#       temptempInstanceData<-temptempInstanceData[kx[1]:maxIndex,]
#       tempInstanceData<-rbind(tempInstanceData,temptempInstanceData)
#       
#       temptempFeatureData<-temptempFeatureData[kx[1]:maxIndex,]
#       tempFeatureData<-rbind(tempFeatureData,temptempFeatureData)
#     }
#   }
#   
#   InstanceData[[i]]<-tempInstanceData
#   rm(tempInstanceData)
#   rm(temptempInstanceData)
#   FeatureData[[i]]<-tempFeatureData
#   rm(tempFeatureData)
#   rm(temptempFeatureData)
#   
#   
#   #cut down Instance data to size of Feature data, or vice versa
#   
#   if(nrow(FeatureData[[i]])<nrow(InstanceData[[i]])){
#     InstanceData[[i]]<-InstanceData[[i]][seq(1,nrow(FeatureData[[i]])),]
#   } else {
#     FeatureData[[i]]<-FeatureData[[i]][seq(1,nrow(InstanceData[[i]])),]
#   }
#   
#   
#   #add participant labels to data
#   InstanceData[[i]]$name<-identifiers[i]
#   FeatureData[[i]]$name<-identifiers[i]
#   
#   
#   #  print(nrow(InstanceData[[i]]))
#   # print(nrow(FeatureData[[i]]))
# }
# 
# 
