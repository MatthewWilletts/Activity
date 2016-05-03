## Clean annotation data for Forest analysis for 10 prototype participants

library(randomForest)
library(data.table)
library(stringr)

#Laptop
if(Sys.info()[['sysname']]=="Darwin"){
dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/'
labelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/'
cleanDataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/clean_data/'
cleanLabelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/Prototype_data/all_participants/clean_data/'
#Linux
} else if(Sys.info()[['sysname']]=='Linux'){
dataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/'
labelDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/'
cleanDataDirectory<-'/data/rockptarmigan/willetts/Prototype_data/clean_data/'
cleanLabelDirectory<-'/data/rockptarmigan/willetts/Prototype_data/all_participants/clean_data/'
}

accelFiles<-list.files(dataDirectory)
accelFiles<-accelFiles[substr(accelFiles, 1, 1)=='p']
labelFiles<-list.files(labelDirectory)
labelFiles<-labelFiles[substr(labelFiles, 1, 3)=='csv']

temp<-sapply(labelFiles, FUN=function(x) {strsplit(x, split='[.]')[[1]][1]})
temp<-as.numeric(sub('csvP','',temp))
ix<-order(temp)
labelFiles<-labelFiles[ix]
keeps<-c('participant','NewStart','NewEnd','label')

rate=50

FirstLast<-NULL


for(i in 1:length(labelFiles)){
labelData<-read.csv(paste0(labelDirectory,labelFiles[i]))
labelData$NewStart<-strptime(labelData$startTime,'%m/%d/%Y %H:%M',tz = 'GMT')
labelData$tempEnd<-strptime(labelData$endTime,'%m/%d/%Y %H:%M',tz = 'GMT')
labelData$duration<-labelData$tempEnd-labelData$NewStart
labelData<-labelData[-(labelData$duration==0),]
labelData$NewEnd<-labelData$tempEnd[length(labelData$NewStart)]
labelData$NewEnd[1:(length(labelData$NewStart)-1)]<-labelData$NewStart[2:(length(labelData$NewStart))]
labelData$NewEnd[length(labelData$NewStart)]<-labelData$tempEnd[length(labelData$NewStart)]
labelData<-labelData[,keeps]
colnames(labelData)<-c('identifier','StartDateTime','EndDateTime','behavior')
if(file.exists(paste0(cleanLabelDirectory,labelData$identifier[1],'.csv'))==FALSE){
write.csv(x=labelData,file=paste0(cleanLabelDirectory,labelData$identifier[1],'.csv'),row.names=FALSE)
}
FirstLast=rbind(FirstLast,data.frame(labelData$identifier[1],labelData$StartDateTime[1],labelData$EndDateTime[length(labelData$identifier)]))
}

colnames(FirstLast)<-c('identifier','First','Last')
FirstLast$Duration<-FirstLast$Last-FirstLast$First
  

#Create complete training chunks of accelerometer data
for (i in 9:length(accelFiles)){
data<-fread(input=paste0(dataDirectory,accelFiles[i]))
start_row<-pmatch(paste0(as.character.POSIXt(FirstLast$First[i]),'.00'),data$V1,duplicates.ok = TRUE)
end_row<-pmatch(paste0(as.character.POSIXt(FirstLast$Last[i]),'.00'),data$V1,duplicates.ok = TRUE)

#delete data points outside labelled epoch
data<-data[start_row:end_row,]

#delete unneccesary columns
data[,V6:=NULL]
data[,V5:=NULL]
data[,V1:=NULL]

data=round(data,3)

#cut down from 100Hz data to new rate (new rate must be a factor of 100)
ind<-seq(1,nrow(data),by=100/rate)
data<-data[ind,]

duration=as.numeric(FirstLast$Duration[i])
#Now create header needed for TLBC
lines=paste0('------------ Data File Created By ActiGraph GT3X+ ActiLife v6.3.0 Firmware v2.0.0 date format M/d/yyyy at ',rate,' Hz  Filter Normal -----------
  Serial Number: MRA1CXXXXXXXX
Start Time ',strftime(FirstLast$First[i],format='%H:%M:%S'),'
Start Date ',strftime(FirstLast$First[i],format='%m/%d/%Y'),'
Epoch Period (hh:mm:ss) ',paste0(str_pad(floor(duration), width=2, pad="0"),':',str_pad(round((duration-floor(duration))*60), width=2, pad="0"),':00'),'
Download Time 00:00:00
Download Date 1/1/2016
Current Memory Address: 0
Current Battery Voltage: 0     Mode = 0
--------------------------------------------------')

if(file.exists(paste0(cleanDataDirectory,FirstLast$identifier[i]))==FALSE){
  write(x=lines,file=paste0(cleanDataDirectory,FirstLast$identifier[i]))
  write.table(x=data,file=paste0(cleanDataDirectory,FirstLast$identifier[i]),append = TRUE,row.names = FALSE,col.names = FALSE,sep=',') 
}


rm (data)

}










