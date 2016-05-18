#View of data

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)

ws=60


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

plotDirectory<-paste0(dataDirectory,'/plots')

tempDirectory<-paste0(dataDirectory,'/temp')

rawAccelDataDir<-file.path(dataDirectory,'rawAccelData')

outputLabelDirectory<-paste0(tempDirectory,'/Bout_Labels_',ws)
outputFeatureDirectory<-paste0(tempDirectory,'/Accel_Features_',ws)


accelFiles<-list.files(rawAccelDataDir)
accelFiles<-accelFiles[substr(accelFiles, 1, 1)=='p']
labelFiles<-list.files(outputLabelDirectory)
labelFiles<-labelFiles[substr(labelFiles, 1, 3)=='csv']

identifiers<-labelFiles


InstanceData<-list()
FeatureData<-list()
for (i in 1:length(identifiers)){
  #Cut Down to certainly correct output

  
  LabelOutputDir<-file.path(outputLabelDirectory,identifiers[i])
  FeatureOutputDir<-file.path(outputFeatureDirectory,identifiers[i])
  
  
  InstanceData[[i]]<-read.csv(file = file.path(LabelOutputDir,list.files(LabelOutputDir)[1]))
  FeatureData[[i]]<-read.csv(file = file.path(FeatureOutputDir,list.files(FeatureOutputDir)[1]))
  
  InstanceData[[i]]$name<-identifiers[i]
  FeatureData[[i]]$name<-identifiers[i]
}


data<-fread(input=file.path(rawAccelDataDir,accelFiles[1]),nrow=)
start_row<-pmatch(paste0(as.character.POSIXt(InstanceData[[1]]$timestamp[1]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-pmatch(paste0(as.character.POSIXt(tail(InstanceData[[1]]$timestamp,1)),'.00'),data$V1,duplicates.ok = TRUE)


data<-data[start_row:end_row,]

#extract a minute of walking data
ix<-which(InstanceData[[1]]$behavior=='walking')
start_row<-pmatch(paste0(as.character.POSIXt(InstanceData[[1]]$timestamp[ix[86]]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-start_row+6000
  
walkingData<-data[start_row:end_row,]

png(paste0(plotDirectory,'/Data',InstanceData[[1]]$behavior[ix[1]],(end_row-start_row)/6000,'ver2min.png'),width = 1000,height = 1000,units = 'px')
p <- ggplot(data = walkingData,aes(x = as.POSIXct(V1))) + 
  geom_line(aes(y = V2, color = "X")) +
  geom_line(aes(y = V3, color = "Y"))  +
  geom_line(aes(y = V4, color = "Z"))  +
  geom_line(aes(y = V5, color = "Mag"))  +
  
  ylab('Acceleration /g') +
  xlab('time /s') +
  labs(color="") +
  ggtitle("Walking 1m")
p
dev.off()


ix<-which(InstanceData[[1]]$behavior=='walking')
start_row<-pmatch(paste0(as.character.POSIXt(InstanceData[[1]]$timestamp[ix[80]]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-start_row+6000*9

longwalkingData<-data[start_row:end_row,]


png(paste0(plotDirectory,'/Data',InstanceData[[1]]$behavior[ix[1]],(end_row-start_row)/6000,'min.png'),width = 1500,height = 1000,units = 'px')
q <- ggplot(data = longwalkingData,aes(x = as.POSIXct(V1))) + 
  geom_line(aes(y = V2, color = "X")) +
  geom_line(aes(y = V3, color = "Y"))  +
  geom_line(aes(y = V4, color = "Z"))  +
  geom_line(aes(y = V5, color = "Mag"))  +
  
  ylab('Acceleration /g') +
  xlab('time') +
  labs(color="") +
  ggtitle("Walking 9m")
q
dev.off()

ix<-which(InstanceData[[1]]$behavior=='walking')
start_row<-pmatch(paste0(as.character.POSIXt(InstanceData[[1]]$timestamp[550]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-start_row+6000*11

longData<-data[start_row:end_row,]
longData$behavior<-''
longData$behavior[1:(6000*6)]<-'sitting'
longData$behavior[(6000*6):nrow(longData)]<-'walking'

png(paste0(plotDirectory,'/DataSittingToWalkingmin.png'),width = 1500,height = 1000,units = 'px')
r = ggplot(longData, aes(x=as.POSIXct(V1),y=V4,color=behavior)) +geom_line()+
  
  ylab('Acceleration /g') +
  xlab('time') +
  labs(color="") +
  ggtitle("Transition sitting to walking")
r
dev.off()


start_row<-pmatch(paste0(as.character.POSIXt(InstanceData[[1]]$timestamp[112]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-start_row+6000*16

longData<-data[start_row:end_row,]
longData$behavior<-''
longData$behavior[1:(6000*6)]<-'walking'
longData$behavior[(6000*6):nrow(longData)]<-'bicycling'

png(paste0(plotDirectory,'/DataWalkingtoBikemin.png'),width = 1500,height = 1000,units = 'px')
r = ggplot(longData, aes(x=as.POSIXct(V1),y=V4,color=behavior)) +geom_line()+
  
  ylab('Acceleration /g') +
  xlab('time') +
  labs(color="") +
  ggtitle("Transition walking to bicycling")
r
dev.off()















transtionFeatures<-FeatureData[[1]][550:560, ]

s = ggplot(transtionFeatures, aes(x=as.POSIXct(timestamp))) + geom_line()+
  ylab('Acceleration /g') +
  xlab('time') +
  labs(color="") +
  ggtitle("walking 1m")


p3 = qplot(x=1:nrow(walkingData),y=walkingData$V4,geom="line")
grid.arrange(p1, p2,p3, ncol = 1, top = "Main title")