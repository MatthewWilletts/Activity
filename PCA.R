#PCA plots of feature data

## further data cleaning and SemiSupervised filling in of 10 initial data points

library(randomForest)
library(data.table)
library(TLBC)
library("upclass")
library(extraTrees)
library(stats)
require(caret)

library(devtools)
install_github("ggbiplot", "vqv")

library(ggbiplot)


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

listOfIndividuals<-list.files(cleanBoutDirectory)
listOfDataFiles<-list.files(cleanDataDirectory)

identifiers<-gsub(listOfIndividuals,pattern = '.csv',replacement = '')

GTinstanceData<-list()
GTfeatureData<-list()


for (i in 1:length(identifiers)){
#Cut Down to certainly correct output
certainlyTrueLabelDirectory<-paste0(dataDirectory,'/CertainlyTrueLabels')
certainlyTrueFeatureDirectory<-paste0(dataDirectory,'/CertainlyTrueFeatures')

certainlyTrueLabelOutputDir<-file.path(certainlyTrueLabelDirectory,identifiers[i])
certainlyTrueFeatureOutputDir<-file.path(certainlyTrueFeatureDirectory,identifiers[i])


GTinstanceData[[i]]<-read.csv(file = file.path(certainlyTrueLabelOutputDir,list.files(certainlyTrueLabelOutputDir)[1]))
GTfeatureData[[i]]<-read.csv(file = file.path(certainlyTrueFeatureOutputDir,list.files(certainlyTrueFeatureOutputDir)[1]))

GTinstanceData[[i]]$name<-identifiers[i]
GTfeatureData[[i]]$name<-identifiers[i]
}


AllGTFeatureData<-do.call("rbind", GTfeatureData)
AllGTInstanceData<-do.call("rbind", GTinstanceData)

#reduce the number of different labels

ReduceGTInstanceData<-reduceLabels(data=AllGTInstanceData,labelsToReduce=list(c('gardening','standing'),c('in-vehicle')),overallLabel =c('sitting','driving'))


#PCA analysis for bicycling
AllDataPCA<-PCAanalysis(AllGTFeatureData,AllGTInstanceData$behavior,AllGTFeatureData)

BicyclingDataPCA<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$name,[AllGTInstanceData$behavior=='bicycling'],resortData = TRUE)

VehicleDataPCA<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$behavior,c(which(AllGTInstanceData$behavior=='in-vehicle'),which(AllGTInstanceData$behavior=='driving')),resortData = TRUE)

GardeningDataPCA<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$behavior,c(which(AllGTInstanceData$behavior=='gardening')),resortData = TRUE)

WalkTransDataPCA<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$behavior,534:544,resortData = TRUE)

BikeTransDataPCA<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$behavior,110:129,resortData = TRUE)


AllDataPCAname<-PCAanalysis(AllGTFeatureData[,2:42],AllGTInstanceData$name)




PCAanalysis<-function(trainingdata,grouping,sortingVector,resortData=FALSE){
    ir.pca <- prcomp(trainingdata,
                   center = TRUE,
                   scale. = TRUE) 
  
    if(resortData){
ir.pca$x<-ir.pca$x[sortingVector,]
grouping<-grouping[sortingVector]
    }
    
    
  g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
                groups = grouping, ellipse = TRUE, 
                circle = TRUE,)
  g <- g + scale_color_discrete(name = '')
  g <- g + theme(legend.direction = 'horizontal', 
                 legend.position = 'top')
return(g)
}
