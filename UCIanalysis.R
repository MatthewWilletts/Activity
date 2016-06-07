#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

library(optparse)
library(randomForest)
library(data.table)
library(RSpectra)
library(mhsmm)
library(parallel)
library(stringr)
library(doMC)
library(foreach)
library(MASS)
library(Rcpp)
library(caret)

source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


ncores<-16

#ncores<-2


set.seed(100)

registerDoMC(ncores)

#First, define data directories

dataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/UCI'
resultsDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/results'

# dataDirectory<-'/Users/Matthew/Documents/Oxford/Activity/FeatureData'
# labelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/LabelData'
# instanceLabelDirectory<-'/Users/Matthew/Documents/Oxford/Activity/InstanceLabelData'
# outputDataDirectory<-dataDirectory
# resultsDataDirectory<-outputDataDirectory


#Directories for RF and HMM models
RFoutput<-paste0(resultsDataDirectory,'/RFoutput')
HMMoutput<-paste0(resultsDataDirectory,'/HMMoutput')

#Directories for predictions
Predictions<-paste0(dataDirectory,'/Predictions')

Data<-read.csv(file.path(dataDirectory,'letter-recognition.data'),header = FALSE)

TrainingData<-Data[1:16000,]
TestingData<-Data[16001:20000,]


TrainingData<-TrainingData[c(which(TrainingData$V1=='A'),which(TrainingData$V1=='B'),which(TrainingData$V1=='C')),]
TrainingData$V1<-factor(TrainingData$V1)
TestingData<-TestingData[c(which(TestingData$V1=='A'),which(TestingData$V1=='B'),which(TestingData$V1=='C')),]
TestingData$V1<-factor(TestingData$V1)

# TrainingData<-Data[1:500,]
# TestingData<-Data[16001:20000,]
# TestingData<-TestingData[1:200,]

  #1.a Run RF using the labelled data points on training data

  ntree=1000
  mtry = floor(sqrt(ncol(TrainingData[,2:17])))
  replace=TRUE
  nodesize=1
  
  cat(paste0('training RF\n'))
  
  cat(paste0('nrow for training data is ',nrow(TrainingData),'\n'))
  cat(paste0('size of features for training data is ',object.size(TrainingData),'\n'))
  
  rf <- foreach(ntree=splitNumber(ntree,nprocs = ncores ), .combine=randomForest::combine, .multicombine=TRUE, .packages='randomForest') %dopar%
    randomForest(x = TrainingData[,2:17],y=as.factor(TrainingData$V1),
                 ntree=ntree,
                 mtry=mtry,
                 replace=replace,
                 nodesize=nodesize,
                 importance=FALSE,
                 proximity = FALSE,
                 do.trace = 100)
  
  
  #1.b we need to know which data point goes to which node of each tree in our training set!
  cat(paste0('extracting training nodes \n'))
  
  training_nodes <- foreach(features=splitMatrix(TrainingData[,2:17],nprocs = ncores),.combine = rbind, .packages='randomForest') %dopar% 
    attr(predict(rf, features, type="prob",
                 norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=TRUE,oob.prox=TRUE),which='nodes')
  
  training_nodes<-training_nodes[order(as.numeric(rownames(training_nodes))),]
  
  #Challenge - create proximity matrix from node locations

  #training_nodes matrix has npoints rows and ntree columns
  
 
  #Proximity matrix is npoints by npoints
  cat(paste0('calculating training nodes proximity \n'))
  
    ProxTrain <- foreach(splitTraining=splitMatrix(training_nodes,nprocs = ncores), .combine = rbind) %dopar% matrix(
                                      computeProximityC(nodes1=training_nodes,nodes2=splitTraining),
                                      nrow=nrow(splitTraining),dimnames=list(rownames(splitTraining)))
  
    ProxTrain<-ProxTrain[order(as.numeric(rownames(ProxTrain))),]
    
  
  #2.Output the RF proximity matrix, for all testing data training data - ie for one participant
  cat(paste0('extracting testing nodes \n'))
  
  testing_nodes <- foreach(features=splitMatrix(TestingData,nprocs = ncores),.combine = rbind, .packages='randomForest') %dopar% attr(
                          predict(rf, features, type="prob",norm.votes=TRUE, predict.all=TRUE,
                          proximity=TRUE, nodes=TRUE),which='nodes')
  
  reordering_indices<-order(as.numeric(row.names(testing_nodes)))
  testing_nodes<-testing_nodes[reordering_indices,]
  
  cat(paste0('extracting RF test predictions \n'))
  
  #and also the RF predicitions
  testing_RF_predicitions<-foreach(features=splitMatrix(TestingData[,2:17],nprocs = ncores),.combine = c, .packages='randomForest') %dopar% 
                            as.character(predict(rf, features, type="response",
                         norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)$aggregate,names)
  
  
  testing_RF_predicitions<-(testing_RF_predicitions[reordering_indices])
  
  
  #Challenge - create proximity matrix from node locations of overlap between Training data and Testing Data
  
  #testing_nodes matrix has ntraingpoints rows and ntree columns
  
  #Proximity matrix is ntestingpoints rows by ntrainingpoints columns

  cat(paste0('calculating testing nodes proximity \n'))
  
  ProxTest <- foreach(splitTesting=splitMatrix(testing_nodes,nprocs = ncores),.combine = rbind) %dopar% matrix(
                         computeProximityC(nodes1=training_nodes,nodes2=splitTesting[,1:ntree]),
                         nrow=nrow(splitTesting),dimnames=list(rownames(splitTesting)))
  
  ProxTest<-ProxTest[order(as.numeric(rownames(ProxTest))),]


  #3.Using ideas from spectral clustering, take an eigen(like) spectral decomposition of ProxTrain and project all
  # testing data points into the leading k-components of the decomposition of ProxTrain
  # with k smallish (say 3 or 4 dimensions)
  
  
  cat(paste0('doing kSpace transform \n'))
  
  Kmax=26
  
  Z<-calcZ(ProxTrain=ProxTrain,Kmax=Kmax,CV=FALSE)
  Z_cv<-calcZ(ProxTrain=ProxTrain,Kmax=Kmax,CV = TRUE)
  
  
  save(Z,file = file.path(resultsDataDirectory,paste0("UCI_Z.RData")))
  save(Z_cv,file = file.path(resultsDataDirectory,paste0("UCI_Z_cv.RData")))
  
  
  #load(file='~/Documents/Oxford/Activity/UCI/UCI_Z.RData')
  LDAperformance<-foreach(k=1:Kmax,.combine = list) %dopar% kSpaceAnalysis(
    kval=k,Z=Z,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
    testing_RF_predicitions=testing_RF_predicitions,resultsDataDirectory=resultsDataDirectory,
    HMMoutput=HMMoutput,RFoutput=RFoutput,outputPrefix = '')
  
  LDAperformance_cv<-foreach(k=1:Kmax,.combine = list) %dopar% kSpaceAnalysis(
    kval=k,Z=Z_cv,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
    testing_RF_predicitions=testing_RF_predicitions,resultsDataDirectory=resultsDataDirectory,
    HMMoutput=HMMoutput,RFoutput=RFoutput,outputPrefix = 'CV')
  

    save(LDAperformance, file = file.path(resultsDataDirectory,paste0("UCIResults.RData")))
    save(LDAperformance_cv, file = file.path(resultsDataDirectory,paste0("UCIResults_cv.RData")))