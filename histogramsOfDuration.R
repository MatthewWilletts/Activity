## Random Forest analysis for 10 prototype participants

library(randomForest)
library(data.table)
library(TLBC)
library(MASS)


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


cleanDataDirectory<-'~/Documents/Oxford/Activity/boutData'
cleanDataFiles<-list.files(cleanDataDirectory)

listOfCleanData<-list()
for(i in 1:length(cleanDataFiles)){
  listOfCleanData[[i]]<-read.csv(paste0(cleanDataDirectory,'/',cleanDataFiles[i]))
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
  
  values<-fitdistr(behaviorBouts$Duration,"geometric")
  sim<-qgeom(ppoints(length(behaviorBouts$Duration)),prob=values$estimate)
  pdf(paste0(plotDirectory,'/QQ_',behaviors[i],'.pdf'),width = 6, height = 6)
  qqplot(x = sim,behaviorBouts$Duration,xlab = 'Theoretical quantiles',ylab = 'Duration quantiles')
  qqline(sim, distribution = function(p) qgeom(p, values$estimate),
         prob = c(0.25, 0.75), col = "red")
  dev.off()
  pdf(paste0(plotDirectory,'/Hist_',behaviors[i],'.pdf'),width = 6, height = 6)
  hist(behaviorBouts$Duration,main =paste0('Histogram of Duration of ',behaviors[i]),xlab='Duration /mins')
  dev.off()
}





