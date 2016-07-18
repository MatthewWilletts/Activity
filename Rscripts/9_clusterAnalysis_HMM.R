#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')
library(plyr)


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will calculate carry out HMM analysis using k space data

#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-8

set.seed(ncores)

registerDoMC(ncores)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')



#ncolFmatrix<-sum(ncol(AllFeatureData[,2:12,with=FALSE]),ncol(AllFFTData[,2:252,with=FALSE]))
AllData<-fread(input = file.path('~/Documents/Oxford/Activity/eigenvectors','AllData_30.csv'))

load(file = file.path('~/Documents/Oxford/Activity/eigenvectors','eigenvectors.RData'))

load(file = file.path('~/Documents/Oxford/Activity/eigenvectors','eigenvectors.RData'))

load(file =file.path('~/Documents/Oxford/Activity/eigenvectors',paste0('participants_',duration,'.RData')))

CVProxTest<-fread(input = file.path(ProxTestOutput,paste0('CVProxTest_',participants[leave_out],'.csv')))
CVProxTest<-as.matrix(CVProxTest)

reduced_CV<-fread(file.path('~/Documents/Oxford/Activity/eigenvectors','ReducedCV.csv'),drop=1)
reduce_prox<-fread(file.path('~/Documents/Oxford/Activity/eigenvectors','ReducedProx.csv'),drop=1)


participant<-participants[1]
  
iq<-which(AllData$identifier==participant)
 
Z<-t(t(Eigenvectors$vectors) * Eigenvectors$values)/ntrees

CVDescriptorFile<-paste0('CVBackingFile_',participants[leave_out],'.desc')

CVProxTrain<-attach.big.matrix(file.path(ProxOutput,CVDescriptorFile))

K_training_data<-CVProxTrain %*% Z

K_training_data<-as.matrix(K_training_data)/ntrees

K_testing_data<-CVProxTest %*% Z/ntrees

write.csv(x = K_training_data,file = file.path(HMMoutput,paste0('K_training_data_',participant,'.csv')))
write.csv(x = K_testing_data,file = file.path(HMMoutput,paste0('K_testing_data_',participant,'.csv')))

K_testing_data<-read.csv('~/Documents/Oxford/Activity/Eigenvectors/K_testing_data_p002.csv')
K_training_data<-read.csv('~/Documents/Oxford/Activity/Eigenvectors/K_training_data_p002.csv')

ix<-which(!AllData$behavior[-iq]=='sitting')
kPlotData<-data.frame(K_training_data[ix,])
kPlotData$X<-NULL
kPlotData$group<-allBehav[ix]
kPlotDatamelt <- melt(kPlotData, id.vars = "group")

ix<-which(AllData$behavior[iq]=='sitting')
kPlotData<-data.frame(K_training_data[ix,])
kPlotData$X<-NULL
kPlotData$group<-AllData$behavior[iq[ix]]
kPlotDatamelt <- melt(kPlotData, id.vars = "group")


ggplot(kPlotData, aes(x=V3,y=V4))+ geom_point(aes(colour = group), size = 1)
ggplot(kPlotDatamelt, aes(x=V1,y=V2)) + geom_boxplot()


#Reduce labels drastically

labels_to_reduce<-list(c("sports" ,"manual-work"))
overallLabel<-c("mixed-activity")
AllData<-reduceLabels(data=AllData_unred,labelsToReduce = labels_to_reduce,overallLabel =overallLabel )

  #4.run an HMM with gaussian emission probs for the projected points in the k-space
  
  ####learn the HMM using the labelled and unlabelled data in the k-space
  cat(paste0('doing HMM for ',participant,'\n'))
  
  

  labelledInstance<-as.factor(AllData$behavior[-iq])
  
  hmmData<-list()
  hmmData$s<-as.numeric(labelledInstance)
  hmmData$x<-t(Z)
  hmmData$N<-length(hmmData$s)
  class(hmmData)<-"hsmm.data"
  
  
  
  
  
  states<-AllData$behavior[-iq]
  states<-states[!is.na(states)]
  
  
  #calculate empirial transition matrix
  statesLength<-length(states)
  Trans<-table(states[1:statesLength-1],states[2:statesLength])
  Trans <- Trans / rowSums(Trans)
  
  unique_states<-levels(as.factor(AllData$behavior))
  
  mu<-list()
  sigma<-list()
  for (j in 1:length(unique_states)){
    mu[[j]]<-colMeans(K_training_data[which(AllData$behavior[-iq]==unique_states[j]),])
    sigma[[j]]<-cov(K_training_data[which(AllData$behavior[-iq]==unique_states[j]),])
  }
  
  initial_state_probabilities<-count(states)
  initial_state_probabilities<-initial_state_probabilities$freq/statesLength
  
  B <- list(mu=mu,sigma=sigma)
  model <- hmmspec(init=initial_state_probabilities, trans = Trans, parms.emis = B,dens.emis = dmvnorm.hsmm)
  
  pred <- matrix(NA, nrow(K_testing_data), 9)
  pred.p <- rep(0, nrow(K_testing_data))
  for(j in 1:9) {
    for(i in 1:nrow(K_testing_data)) {
      p <- dmvnorm(K_testing_data[i,],mu[[j]],sigma[[j]])
      #if(p>pred.p[i]) {
      #  pred[i] <- j
      #  pred.p[i] <- p
      #}
      pred[i,j] <- p
    }
  }
  
  
  save(model, file = file.path(HMMoutput,paste0(participant,paste0('HMMmodel',participant,'.R'))))
  
  ##Now train model
  
  #output<-hmmfit(x = hmmData,start.val = model,mstep=mstep.mvnorm,lock.transition=FALSE,tol=1e-08,maxit=1000)
  
  #train <- simulate(model,  nsim=100, seed=1234, rand.emis=rmvnorm.hsmm)
  cat(paste0('predicting HMM for ',participant,'\n'))
  
  smoothed<-predict(object = model,newdata = (K_testing_data),method = 'viterbi')
  
  newLabels<-factor(smoothed$s)
  
  labelCode<-levels(as.factor(AllData$behavior))
  
  labelCode_beginning<-labelCode[as.numeric(levels(newLabels))]
  

  labelCode<-c(labelCode_beginning,setdiff(labelCode,labelCode_beginning))
  
  
  levels(newLabels)<-labelCode
  
  true_reference<-factor(AllData$behavior[iq])
 
   levels(true_reference)<-c(levels(true_reference),setdiff(levels(as.factor(AllData$behavior)),levels(true_reference)))
  
  
  #newLabels<-as.character(newLabels)
  
  #Calculate Confusion matrix
  
  HMM_confusion_matrix<-confusionMatrix(data =newLabels,reference =  true_reference)
  
  #output LDA and HMM confusion matrices
  
  LDAperformance[[i]]<-lda_RF_confusion_matrix
  HMMperformance[[i]]<-HMM_confusion_matrix
  
  #write predicitions
  cat(paste0('saving predictions for ',participant,'\n'))
  
  
  write.csv(x=testing_RF_predicitions,file = file.path(RFoutput,paste0(participant,'RFpred.csv')))
  write.csv(x=newLabels,file = file.path(HMMoutput,paste0(participant,'HMMpred.csv')))
  write.csv(x=AllInstanceData$behavior[ix[iq]],file = file.path(HMMoutput,paste0(participant,'true.csv')))
  
  
}

save(LDAperformance, HMMperformance, file = file.path(resultsDataDirectory,"Results.RData"))


#LOAD RFs
list_of_rfs<-paste0('RF_',1:10,'_p002.RData')

load(file.path(RFoutput,list_of_rfs[1]))
RF<-rf
for(i in 2:10){
  load(file.path(RFoutput,list_of_rfs[1]))
  RF<-combine(RF,rf)
  }
