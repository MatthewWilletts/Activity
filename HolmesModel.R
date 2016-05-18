## ANALYSIS USING HOLMES ALGO

set.seed(12345)


library(randomForest)
library(data.table)
library(TLBC)
library("upclass")
library(extraTrees)
library(RSpectra)
library(mhsmm)

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

#Load up Data
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


  #Step 1 - Load up all data
  
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
  

#cut down Instance data to size of Feature data, or vice versa

  if(nrow(FeatureData[[i]])<nrow(InstanceData[[i]])){
    InstanceData[[i]]<-InstanceData[[i]][seq(1,nrow(FeatureData[[i]])),]
  } else {
    FeatureData[[i]]<-FeatureData[[i]][seq(1,nrow(InstanceData[[i]])),]
  }


#add participant labels to data
  InstanceData[[i]]$name<-identifiers[i]
  FeatureData[[i]]$name<-identifiers[i]
  

#  print(nrow(InstanceData[[i]]))
# print(nrow(FeatureData[[i]]))
}


AllFeatureData<-do.call("rbind", FeatureData)
AllInstanceData<-do.call("rbind", InstanceData)

ReduceInstanceData<-reduceLabels(data=AllInstanceData,labelsToReduce=list(c('gardening','standing'),c('in-vehicle')),overallLabel =c('sitting','driving'))

#1. Run RF using the labelled data points

ix<-which(!AllInstanceData$behavior=='nolabel')


ntree=500
mtry = floor(sqrt(ncol(AllFeatureData[ix,2:42])))
replace=TRUE
nsample=1000
nodesize=1
sampsize=1000

rf<-randomForest(x = AllFeatureData[ix,2:42],y=as.factor(ReduceInstanceData$behavior[ix]),
             ntree=ntree,
             mtry=mtry,
             replace=replace,
             sampsize=sampsize,
             nodesize=nodesize,
             importance=TRUE,
             proximity = TRUE,
             do.trace = TRUE)

#2.Output the RF proximity matrix, D, for all data points labelled and unlabelled

rf.predict<-predict(rf, AllFeatureData[,2:42], type="prob",
                    norm.votes=TRUE, predict.all=FALSE, proximity=TRUE, nodes=FALSE)

D<-rf.predict$proximity

#3.Using ideas from spectral clustering, take an eigen(like) spectral decomposition of D and project all
# (labelled and unlabelled) data points into the leading k-components of the decomposition of D
# with k smallish (say 3 or 4 dimensions)

Diag <- diag(apply(D, 1, sum))
U<-Diag-D

k   <- length(unique(ReduceInstanceData$behavior))
evL <- eigs_sym(U,k+1,which='SM')
Z   <- evL$vectors[,1:k]

plot(Z, col=as.factor(ReduceInstanceData$behavior[ix]), pch=20)


#4.run an HMM with gaussian emission probs for the projected points in the k-space
#learn the HMM using the labelled and unlabelled data in the k-space


ReduceInstanceDataNAs<-ReduceInstanceData
ReduceInstanceDataNAs[ReduceInstanceData[,2]=='nolabel',2]<-NA

labelledInstance<-as.factor(as.vector(ReduceInstanceDataNAs[,2]))

hmmData<-list()
hmmData$s<-as.numeric(labelledInstance)
hmmData$x<-Z
hmmData$N<-length(hmmData$s)
class(hmmData)<-"hsmm.data"





states<-unique(ReduceInstanceDataNAs$behavior)
states<-states[!is.na(states)]
J <- length(states)
init <- c(1, rep(0,times=J-1))
P <- matrix(c(10,1,1,1,1,10,1,1,1,1,10,1,1,1,1,10), nrow = J)/(10+(J-1))

mu<-list()
sigma<-list()
for (i in 1:J){
  mu[[i]]<-colMeans(Z[which(ReduceInstanceData$behavior==states[i]),])
  sigma[[i]]<-cov(Z[which(ReduceInstanceData$behavior==states[i]),])
  }
    
B <- list(mu=mu,sigma=sigma)
model <- hmmspec(init=init, trans = P, parms.emis = B,dens.emis = dmvnorm.hsmm)

#Now train model


output<-hmmfit(x = hmmData,start.val = model,mstep=mstep.mvnorm,lock.transition=FALSE,tol=1e-08,maxit=1000)

#train <- simulate(model,  nsim=100, seed=1234, rand.emis=rmvnorm.hsmm)

smoothed<-predict(object = output$model,newdata = Z,method = 'viterbi',)

newLabels<-as.factor(smoothed$s)

labelledInstance<-as.factor(as.vector(ReduceInstanceData[ix,2]))
labelCode<-levels(labelledInstance)


levels(newLabels)<-labelCode
newLabels<-as.character(newLabels)




hsmmfit(train, startval, mstep = mstep.mvnorm, M = 200)
R> summary(hmv)


