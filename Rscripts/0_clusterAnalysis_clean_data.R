#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

library(stringr)
library(doMC)
library(foreach)
library(MASS)
library(Rcpp)

#In this script we will be doing 'leave one out' analysis for our participants

ncores<-16

#ncores<-2


set.seed(1)

registerDoMC(ncores)

duration=30

source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#This script will clean data and save a list of participants and a .csv data file.



#First, define data directories
dataDirectory<-'/data/dph-ukbaccworkgroup/phpc0595/data/capture24/epoch30sec'
labelDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/label-data/label-dictionary-9-classes'
instanceLabelDirectory<-'/data/dph-ukbaccworkgroup/magd4534/label-data/instance-label-dictionary-9-classes'
outputDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/capture-processed/30sec'
resultsDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/results/30sec'

# dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/FeatureData'
# labelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/LabelData'
# instanceLabelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/InstanceLabelData'
# outputDataDirectory<-dataDirectory
# resultsDataDirectory<-outputDataDirectory


#Directories for RF and HMM models
RFoutput<-paste0(resultsDataDirectory,'/RFoutput')
HMMoutput<-paste0(resultsDataDirectory,'/HMMoutput')


listOfFeatureFiles<-list.files(dataDirectory,pattern = "\\ActivityEpoch[.]csv$")
#listOfFFTFiles<-list.files(dataDirectory,pattern = "\\ActivityEpoch_fft[.]csv$")

listOfLabelFiles<-list.files(labelDirectory,pattern = "\\.csv$")


labelIdentifiers<-gsub(listOfLabelFiles,pattern = ".csv$",replacement = '')
labelIdentifiers<-gsub(labelIdentifiers,pattern = "[P]",replacement ='')
listOfLabelFiles<-listOfLabelFiles[order(as.numeric(labelIdentifiers))]
labelIdentifiers<-labelIdentifiers[order(as.numeric(labelIdentifiers))]
labelIdentifiers<-sprintf("p%03d", as.numeric(labelIdentifiers))


featureIdentifiers<-gsub(listOfFeatureFiles,pattern = "ActivityEpoch.csv$",replacement = '')

#Now find which participants have both feature and label data
bothData<-intersect(labelIdentifiers,featureIdentifiers)
iLabels<-match(bothData,labelIdentifiers)
iFeatures<-match(bothData,featureIdentifiers)

jointLabelFiles<-listOfLabelFiles[iLabels]
jointFeatureFiles<-listOfFeatureFiles[iFeatures]
#jointFFTFiles<-listOfFFTFiles[iFeatures]
jointInstanceFiles<-gsub(listOfFeatureFiles[iFeatures],pattern = "ActivityEpoch",replacement = '')

#jointFiles<-mapply(c, jointLabelFiles, jointFeatureFiles,jointInstanceFiles,jointFFTFiles, SIMPLIFY=FALSE)
jointFiles<-mapply(c, jointLabelFiles, jointFeatureFiles,jointInstanceFiles, SIMPLIFY=FALSE)


InstanceData<-list()
FeatureData<-list()

#Now load up Instance data and Feature Data
dropvals<-c(4)

Data<-mclapply(X = jointFiles,FUN = function(x) try(cleanData(jointFiles = x,
                                                              drop=dropvals,
                                                              instanceLabelDirectory = instanceLabelDirectory,
                                                              labelDirectory = labelDirectory,
                                                              dataDirectory = dataDirectory,
                                                              outputDataDirectory=outputDataDirectory,onlyLoad = FALSE,FFT=FALSE,duration=duration))
               ,mc.cores = ncores)

#remove errored participants
deleteParticipants<-(which(sapply(Data,length)==1))
Data[deleteParticipants]<-NULL



#AllFeatureData<-rbindlist(Data %>% map(c("labelledData", "labelledFeatureData")))
#AllFFTData<-rbindlist(Data %>% map(c("labelledData", "labelledFFTData")))

AllFeatureData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"labelledFeatureData"))
# AllFeatureData$dataErrors<-NULL
# AllFeatureData$clipsBeforeCalibr<-NULL
# AllFeatureData$clipsAfterCalibr<-NULL
# AllFeatureData$rawSamples<-NULL
# AllFeatureData$samples<-NULL
#AllFeatureData[,2:12,with=FALSE]<-round(AllFeatureData[,2:12,with=FALSE])


#AllFFTData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"labelledFFTData"))
#AllFFTData[,2:251,with=FALSE]<-round(AllFFTData[,2:251,with=FALSE], digits = 4)


AllInstanceData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"instanceLabelData"))


#AllData<-(cbind(AllInstanceData[,1:3,with=FALSE],AllFeatureData[,2:12,with=FALSE],AllFFTData[,2:251,with=FALSE]))
AllData<-(cbind(AllInstanceData[,1:3,with=FALSE],AllFeatureData[,2:ncol(AllFeatureData),with=FALSE]))


#rm(AllFFTData)
rm(AllInstanceData)
rm(AllFeatureData)

rm(Data)

#For now, only work with labelled data
AllData<-AllData[which(!AllData$behavior=='unknown'),]

#Create testing data - leave one out
participants<-sapply(X = jointInstanceFiles[-deleteParticipants], function (x) gsub(pattern = '.csv',replacement = '',x = x))


#write data
write.csv(x=AllData,file = file.path(outputDataDirectory,paste0('AllData_',duration,'.csv')))

#write participants
save(participants,file =file.path(resultsDataDirectory,'_participants_',duration,'.RData'))
