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
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


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
                                                              instanceLabelDirectory = instanceLabelDirectory,
                                                              labelDirectory = labelDirectory,
                                                              dataDirectory = dataDirectory,
                                                              outputDataDirectory=outputDataDirectory,onlyLoad = TRUE,FFT=FALSE,duration=duration))
               ,mc.cores = ncores)


any_deleted_participants<-FALSE
#remove errored participants
if(length((which(sapply(Data,length)==1))>1)){
deleteParticipants<-(which(sapply(Data,length)==1))
Data[deleteParticipants]<-NULL
any_deleted_participants<-TRUE
}


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

#Fix NAs in AllData
AllData<-FixNAs(AllData)


#Create testing data - leave one out
if(any_deleted_participants==TRUE){
participants<-sapply(X = jointInstanceFiles[-deleteParticipants], function (x) gsub(pattern = '.csv',replacement = '',x = x))
} else {
participants<-sapply(X = jointInstanceFiles, function (x) gsub(pattern = '.csv',replacement = '',x = x))
}
cat('writing AllData file')
#write data
write.csv(x=AllData,file = file.path(outputDataDirectory,paste0('AllData_',duration,'.csv')))

cat('writing participants')
#write participants
save(participants,file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))
