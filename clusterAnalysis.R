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

source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


ncores<-16

#ncores<-2


set.seed(100)

registerDoMC(ncores)

#First, define data directories

dataDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/capture-processed'
labelDirectory<-'/data/dph-ukbaccworkgroup/npeu0203/label-data/label-dictionary-9-classes'
instanceLabelDirectory<-'/data/dph-ukbaccworkgroup/magd4534/label-data/instance-label-dictionary-9-classes'
outputDataDirectory<-'/data/dph-ukbaccworkgroup/magd4534/capture-processed'
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


listOfFeatureFiles<-list.files(dataDirectory,pattern = "\\ActivityEpoch[.]csv$")
listOfFFTFiles<-list.files(dataDirectory,pattern = "\\ActivityEpoch_fft[.]csv$")

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
jointFFTFiles<-listOfFFTFiles[iFeatures]
jointInstanceFiles<-gsub(listOfFeatureFiles[iFeatures],pattern = "ActivityEpoch",replacement = '')

jointFiles<-mapply(c, jointLabelFiles, jointFeatureFiles,jointFFTFiles,jointInstanceFiles, SIMPLIFY=FALSE)


InstanceData<-list()
FeatureData<-list()

#Now load up Instance data and Feature Data
dropvals<-c(4)

Data<-mclapply(X = jointFiles,FUN = function(x) try(cleanData(jointFiles = x,
                                                        drop=dropvals,
                                                        instanceLabelDirectory = instanceLabelDirectory,
                                                        labelDirectory = labelDirectory,
                                                        dataDirectory = dataDirectory,
                                                        outputDataDirectory=outputDataDirectory,onlyLoad = TRUE))
                                                        ,mc.cores = ncores)

#remove errored participants
deleteParticipants<-(which(sapply(Data,length)==1))
Data[deleteParticipants]<-NULL


LDAperformance<-list()
HMMperformance<-list()


#AllFeatureData<-rbindlist(Data %>% map(c("labelledData", "labelledFeatureData")))
#AllFFTData<-rbindlist(Data %>% map(c("labelledData", "labelledFFTData")))

AllFeatureData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"labelledFeatureData"))
AllFeatureData$dataErrors<-NULL
AllFeatureData$clipsBeforeCalibr<-NULL
AllFeatureData$clipsAfterCalibr<-NULL
AllFeatureData$rawSamples<-NULL
AllFeatureData$samples<-NULL


AllFFTData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"labelledFFTData"))

#ncolFmatrix<-sum(ncol(AllFeatureData[,2:12,with=FALSE]),ncol(AllFFTData[,2:252,with=FALSE]))
AllFeatureData<-(cbind(AllFeatureData[,2:12,with=FALSE],AllFFTData[,2:251,with=FALSE]))

rm(AllFFTData)

#ncol = ncolFmatrix, byrow = TRUE)
#AllFeatureData2<-mmult1(as.matrix(AllFeatureData[,2:12,with=FALSE]),as.matrix(AllFFTData[,2:252,with=FALSE]))

#AllInstanceData<-rbindlist(Data %>% map(c("labelledData", "instanceLabelData")))

AllInstanceData<-rbindlist(lapply(X = lapply(X=Data,'[[',"labelledData"), '[[',"instanceLabelData"))

rm(Data)

#behaviorAsFactor<-as.factor(AllInstanceData$behavior)


#ReduceInstanceData<-reduceLabels(data=AllInstanceData,labelsToReduce=list(c('gardening','standing'),c('in-vehicle')),overallLabel =c('sitting','driving'))


#For now, only work with labelled data
ix<-which(!AllInstanceData$behavior=='unknown')


#Create testing data - leave one out
participants<-lapply(X = jointInstanceFiles[-deleteParticipants], function (x) gsub(pattern = '.csv',replacement = '',x = x))
behaviors<-unique(AllInstanceData$behavior)

for(participant in participants){
  
  
iq<-which(AllInstanceData$identifier[ix]==participant)

  
#1.a Run RF using the labelled data points for all but one participant's data

ntree=1000
mtry = floor(sqrt(ncol(AllFeatureData[ix,])))
replace=TRUE
nodesize=1

cat(paste0('training RF for ',participant,'\n'))

cat(paste0('nrow for ',participant,' is ',nrow(AllFeatureData[ix[-iq]]),'\n'))
cat(paste0('size of features for ',participant,' is ',object.size(AllFeatureData[ix[-iq]]),'\n'))

rf <- foreach(ntree=rep(floor(100/ncores),ncores), .combine=randomForest::combine, .packages='randomForest') %dopar%
  randomForest(x = AllFeatureData[ix[-iq],],y=as.factor(AllInstanceData$behavior[ix[-iq]]),
                 ntree=ntree,
                 mtry=mtry,
                 replace=replace,
                 nodesize=nodesize,
                 importance=FALSE,
                 proximity = FALSE,
                 do.trace = 10)


#1.b we need to know which data point goes to which node of each tree in our training set!
cat(paste0('extracting training nodes for ',participant,'\n'))
    
training_nodes <- foreach(features=split(AllFeatureData[ix[-iq],],1:ncores),.combine = rbind, .packages='randomForest') %dopar% 
          attr(predict(rf, features, type="prob",
                  norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=TRUE,oob.prox=TRUE),which='nodes')

training_nodes<-training_nodes[order(as.numeric(row.names(training_nodes))),]

#Challenge - create proximity matrix from node locations

#training_nodes matrix has npoints rows and ntree columns

#Proximity matrix is npoints by npoints
cat(paste0('calculating training nodes proximity for ',participant,'\n'))
    
ProxTrain<-computeProximity(nodes1=training_nodes,nodes2=training_nodes,parallel = TRUE,mc.cores = ncores)


#2.Output the RF proximity matrix, for all testing data training data - ie for one participant
cat(paste0('extracting testing nodes for ',participant,'\n'))
    
testing_nodes <- foreach(features=split(AllFeatureData[ix[iq],],1:ncores),.combine = rbind, .packages='randomForest') %dopar% 
        attr(predict(rf, features, type="prob",
               norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=TRUE),which='nodes')

reordering_indices<-order(as.numeric(row.names(testing_nodes)))
testing_nodes<-testing_nodes[reordering_indices,]

cat(paste0('extracting RF test predictions for ',participant,'\n'))

#and also the RF predicitions
testing_RF_predicitions<-foreach(features=split(AllFeatureData[ix[iq],],1:ncores),.combine = c, .packages='randomForest') %dopar% 
        as.character(predict(rf, features, type="response",
               norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)$aggregate,names)


testing_RF_predicitions<-(testing_RF_predicitions[reordering_indices])


#Challenge - create proximity matrix from node locations of overlap between Training data and Testing Data

#testing_nodes matrix has ntraingpoints rows and ntree columns

#Proximity matrix is ntestingpoints rows by ntrainingpoints columns

cat(paste0('calculating testing nodes proximity for ',participant,'\n'))
    
ProxTest<-computeProximity(nodes1=testing_nodes,nodes2=training_nodes,parallel = TRUE,mc.cores = ncores)



#3.Using ideas from spectral clustering, take an eigen(like) spectral decomposition of ProxTrain and project all
# testing data points into the leading k-components of the decomposition of ProxTrain
# with k smallish (say 3 or 4 dimensions)

cat(paste0('doing kSpace transform for ',participant,'\n'))

Diag <- diag(apply(ProxTrain, 1, sum))
U<-Diag-ProxTrain

k   <- length(behaviors)
evL <- eigs_sym(U,k+1,which='SM')
Z   <- t(evL$vectors[,1:k])

#Z is our projection operator

#plot(Z, col=as.factor(AllInstanceData$behavior[ix]), pch=20)


#Now project our testing data into K space

kData<-Z %*% ProxTest

#Intermediate step:
#compare the out-of-sample classification performance of LDA trained on {labels, z}
#to the original RF trained on {labels, featuredata}, and as a function of k

cat(paste0('doing LDA for ',participant,'\n'))

lda_comparison<-lda(x=t(Z),grouping = as.factor(AllInstanceData$behavior[ix[-iq]]))


lda_prediction<-predict(object = lda_comparison,newdata=t(kData),dimen = k)

reference<-factor(testing_RF_predicitions,levels = levels(lda_prediction$class))

lda_RF_confusion_matrix<-confusionMatrix(data =lda_prediction$class,reference = reference)


#4.run an HMM with gaussian emission probs for the projected points in the k-space

####learn the HMM using the labelled and unlabelled data in the k-space
cat(paste0('doing HMM for ',participant,'\n'))


AllInstanceData$behavior[-ix]<-NA

labelledInstance<-as.factor(AllInstanceData$behavior[ix[-iq]])

hmmData<-list()
hmmData$s<-as.numeric(labelledInstance)
hmmData$x<-t(Z)
hmmData$N<-length(hmmData$s)
class(hmmData)<-"hsmm.data"





states<-AllInstanceData$behavior[ix[-iq]]
states<-states[!is.na(states)]


#calculate empirial transition matrix
statesLength<-length(states)
Trans<-table(states[1:statesLength-1],states[2:statesLength])
Trans <- Trans / rowSums(Trans)

mu<-list()
sigma<-list()
for (j in 1:J){
  mu[[j]]<-colMeans(Z[which(AllInstanceData$behavior==states[j]),])
  sigma[[j]]<-cov(Z[which(AllInstanceData$behavior==states[j]),])
}

B <- list(mu=mu,sigma=sigma)
model <- hmmspec(init=init, trans = Trans, parms.emis = B,dens.emis = dmvnorm.hsmm)

save(model,rf, file = file.path(resultsDataDirectory,paste0(participant,'HMMandRFmodel.R')))

##Now train model

#output<-hmmfit(x = hmmData,start.val = model,mstep=mstep.mvnorm,lock.transition=FALSE,tol=1e-08,maxit=1000)

#train <- simulate(model,  nsim=100, seed=1234, rand.emis=rmvnorm.hsmm)
cat(paste0('predicting HMM for ',participant,'\n'))

smoothed<-predict(object = model,newdata = kData,method = 'viterbi')

newLabels<-factor(smoothed$s)

labelCode<-levels(as.factor(AllInstanceData$behavior[ix]))

true_reference<-factor(AllInstanceData$behavior[ix[iq]],levels = labelCode)
                  

levels(newLabels)<-labelCode
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
