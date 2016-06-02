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
  
  cat(paste0('doing kSpace transform for \n'))
  
  Diag <- diag(apply(ProxTrain, 1, sum))
  U<-Diag-ProxTrain
  
  k   <- 5
  evL <- eigs_sym(U,k+1,which='SM')
  Z   <- evL$vectors[,1:k]
  
  #Z is our projection operator
  
  #plot(Z, col=as.factor(AllInstanceData$behavior[ix]), pch=20)
  
  
  #Now project our testing data into K space
  
  kData<-t(ProxTest %*% Z)
  
  #Intermediate step:
  #compare the out-of-sample classification performance of LDA trained on {labels, z}
  #to the original RF trained on {labels, featuredata}, and as a function of k
  
  cat(paste0('doing LDA \n'))
  
  lda_comparison<-lda(x=Z,grouping = as.factor(TrainingData[,1]))
  
  
  lda_prediction<-predict(object = lda_comparison,newdata=t(kData),dimen = k)
  
  reference<-factor(testing_RF_predicitions,levels = levels(lda_prediction$class))
  
  lda_RF_confusion_matrix<-confusionMatrix(data =lda_prediction$class,reference = reference)
  
  
  # #4.run an HMM with gaussian emission probs for the projected points in the k-space
  # 
  # ####learn the HMM using the labelled and unlabelled data in the k-space
  # cat(paste0('doing HMM for \n'))
  # 
  # 
  # 
  # labelledInstance<-as.factor(TrainingData[,1])
  # 
  # hmmData<-list()
  # hmmData$s<-as.numeric(TrainingData[,2:17])
  # hmmData$x<-t(Z)
  # hmmData$N<-length(hmmData$s)
  # class(hmmData)<-"hsmm.data"
  # 
  # 
  # 
  # 
  # 
  # states<-TrainingData[,1]
  # states<-states[!is.na(states)]
  # 
  # 
  # #calculate empirial transition matrix
  # statesLength<-length(states)
  # Trans<-table(states[1:statesLength-1],states[2:statesLength])
  # Trans <- Trans / rowSums(Trans)
  # 
  # labelCode<-levels(as.factor(TrainingData[,1]))
  # 
  # mu<-list()
  # sigma<-list()
  # for (j in 1:length(labelCode)){
  #   mu[[j]]<-colMeans(Z[which(TrainingData[,1]==labelCode[j]),])
  #   sigma[[j]]<-cov(Z[which(TrainingData[,1]==labelCode[j]),])
  # }
  # 
  # B <- list(mu=mu,sigma=sigma)
  # model <- hmmspec(init=init, trans = Trans, parms.emis = B,dens.emis = dmvnorm.hsmm)
  # 
  # save(model,rf, file = file.path(resultsDataDirectory,paste0(participant,'UCI_HMMandRFmodel.R')))
  # 
  # ##Now train model
  # 
  # #output<-hmmfit(x = hmmData,start.val = model,mstep=mstep.mvnorm,lock.transition=FALSE,tol=1e-08,maxit=1000)
  # 
  # #train <- simulate(model,  nsim=100, seed=1234, rand.emis=rmvnorm.hsmm)
  # cat(paste0('predicting HMM \n'))
  # 
  # smoothed<-predict(object = model,newdata = kData,method = 'viterbi')
  # 
  # newLabels<-factor(smoothed$s)
  # 
  # true_reference<-factor(TestingData[,1],levels = labelCode)
  # 
  # 
  # levels(newLabels)<-labelCode
  #newLabels<-as.character(newLabels)
  
  #Calculate Confusion matrix
  
 # HMM_confusion_matrix<-confusionMatrix(data =newLabels,reference =  true_reference)
  
  #output LDA and HMM confusion matrices
  
  LDAperformance<-lda_RF_confusion_matrix
 # HMMperformance[[i]]<-HMM_confusion_matrix
  
  #write predicitions
  cat(paste0('saving predictions \n'))
  
  
  write.csv(x=testing_RF_predicitions,file = file.path(RFoutput,'UCI_RFpred.csv'))
  write.csv(x=lda_prediction,file = file.path(RFoutput,'UCI_LDApred.csv'))
  #write.csv(x=newLabels,file = file.path(HMMoutput,'HMMpred.csv'))
  write.csv(x=TrainingData[,1],file = file.path(HMMoutput,'UCI_true.csv'))
  
  
  

#save(LDAperformance, HMMperformance, file = file.path(resultsDataDirectory,"Results.RData"))

save(LDAperformance, file = file.path(resultsDataDirectory,"Results.RData"))
