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

plotDirectory<-paste0(dataDirectory,'/plots')


cleanDataFiles<-list.files(cleanDataDirectory)

listOfCleanData<-list()
for(i in 1:length(cleanDataFiles)){
  listOfCleanData[[i]]<-fread(input = paste0(cleanDataDirectory,'/',cleanDataFiles[i]))
}

cleanData<-rbindlist((listOfCleanData))


cleanBoutFiles<-list.files(cleanBoutDirectory)

listOfCleanBouts<-list()
for(i in 1:length(cleanBoutFiles)){
  listOfCleanBouts[[i]]<-fread(input = paste0(cleanBoutDirectory,'/',cleanBoutFiles[i]))
}

cleanBouts<-rbindlist((listOfCleanBouts))

cleanBouts$Duration<-as.numeric(as.POSIXct(cleanBouts$EndDateTime)-as.POSIXct(cleanBouts$StartDateTime))/60
cleanBouts<-cleanBouts[cleanBouts$Duration>0,]

behaviors<-unique(cleanBouts$behavior)
for(i in 1:length(behaviors)){
  behaviorBouts<-cleanBouts[cleanBouts$behavior==behaviors[i],]
  png(paste0(plotDirectory,'/Hist_',behaviors[i],'.png'))
  hist(behaviorBouts$Duration,main =paste0('Histogram of Duration of ',behaviors[i],' for initial data'))
  dev.off()
}





