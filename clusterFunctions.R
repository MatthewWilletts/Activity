  #Function for cleaning up raw bout data to machine-readible format for analysis
#cleanData <- function(jointFiles=jointFiles,labelDirectory=labelDirectory,dataDirectory=dataDirectory,drop=dropvals,outputLabelDir=outputLabelDir,instanceLabelDirectory=instanceLabelDirectory){

cleanData <- function(jointFiles,labelDirectory,dataDirectory,drop=dropvals,outputLabelDir,instanceLabelDirectory,outputDataDirectory,onlyLoad=FALSE){
  
  
  participantID<-gsub(jointFiles[4],pattern = ".csv",replacement = '')
  cat(paste0('starting data processing/loading for ', participantID,'\n'))
  
  if((file.exists(file.path(instanceLabelDirectory,paste0(participantID,'Clean.csv')))==FALSE) & (onlyLoad==FALSE)){
    
    labelData<-read.csv(file.path(labelDirectory,jointFiles[1]))
    keeps <- c('participant','label','startTime','endTime')
    labelData<-labelData[ , (names(labelData) %in% keeps)]
    
    labelData$NewStart<-(as.POSIXct(labelData$startTime,'%Y-%m-%d %H:%M:%S',tz = 'GMT'))
    labelData$tempEnd<-as.POSIXct(labelData$endTime,'%Y-%m-%d %H:%M:%S',tz = 'GMT')
    
    labelData$duration<-labelData$tempEnd-labelData$NewStart
    if(sum(labelData$duration==0)>0){
      labelData<-labelData[-(labelData$duration==0),]
    }
    labelData$NewEnd<-labelData$tempEnd
    
    checklabelData(checkData = labelData)
    
    nrows<-nrow(labelData)
    #Now relabel start and end points to lie in the midpoint of unlabelled gaps
    
    
    labelData$NewEnd[1:nrows-1]<-labelData$NewEnd[1:nrows-1]+floor(labelData$NewStart[2:nrows]-labelData$NewEnd[1:nrows-1])/2
    labelData$NewEnd[nrows]<-labelData$tempEnd[nrows]
    
    labelData$NewStart[2:nrows]<-labelData$NewEnd[1:nrows-1]
    

    labelData<-labelData[,c('participant','label','NewStart','NewEnd')]
    setnames(labelData,c('identifier','behavior','StartDateTime','EndDateTime'))
    
    labelData$identifier<-participantID
      
      
    #Load up Feature Data
    featureData<-fread(file.path(dataDirectory,jointFiles[2]))
    featureData$identifier<-participantID
    #Load up FFT Data
    FFTData<-fread(file.path(dataDirectory,jointFiles[3]))
    FFTData$identifier<-participantID
    
    
    
    #remove fractional seconds
    featureData$Time<-substr(featureData$Time,1,19)
    FFTData$Time<-substr(FFTData$Time,1,19)
    
    
    start_times<-labelData$StartDateTime[1]+0:4
    
    end_times<-labelData$EndDateTime[nrows]-0:4
    
    start_row<-pmatch(start_times,featureData$Time,duplicates.ok = TRUE)
    iStart<-which(!is.na(start_row))
    start_row<-start_row[iStart]
    labelData$StartDateTime[1]<-start_times[iStart]
    
    end_row<-pmatch(end_times,substr(featureData$Time,1,19),duplicates.ok = TRUE)
    iEnd<-which(!is.na(end_row))
    end_row<-end_row[iEnd]
    labelData$EndDateTime[nrows]<-end_times[iEnd]
    
    labelledData<-list()
    
    #only store data points inside labelled epoch
    # minus one as extractLabelsSingleFile does not output final data point for labels
    labelledData$labelledFeatureData<-featureData[start_row:(end_row-1),]
    labelledData$labelledFFTData<-FFTData[start_row:(end_row-1),]
    
    #now round labels to align with spacing of features
    labelData$StartDateTime<-round5secs(labelData$StartDateTime,startTime = labelData$StartDateTime[1])
    labelData$EndDateTime<-round5secs(labelData$EndDateTime,startTime = labelData$StartDateTime[1])
    
    #now we need to turn from bout level labels to annotations. Here we use a slightly modified TLBC function, annotationsToLabels
    extractLabelsSingleFile(labelData, identifier= participantID,winSize = 5,instanceLabelDirectory = instanceLabelDirectory)
    
    labelledData$instanceLabelData<-fread(file.path(instanceLabelDirectory, paste0(participantID,'ALL.csv')))
    labelledData$instanceLabelData$identifier<-participantID
    
    cat(paste0('Writing ALL feature data file ', participantID,'.csv','\n'))
    write.csv(x=labelledData$labelledFeatureData,file=paste0(outputDataDirectory,'/', participantID,'ALLFeature.csv'),row.names=FALSE)
    
    cat(paste0('Writing ALL FFT data file ', participantID,'.csv','\n'))
    write.csv(x=labelledData$labelledFFTData,file=paste0(outputDataDirectory,'/', participantID,'ALLFFT.csv'),row.names=FALSE)

    #remove unknown labels at beginning and end of period - this is the 'clean' data
    cleanData<-removeUnlabelledEnds(data=labelledData)
   
    cat(paste0('Writing CLEAN instance data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanInstanceLabelData,file=paste0(instanceLabelDirectory,'/', participantID,'Clean.csv'),row.names=FALSE)
    
    cat(paste0('Writing CLEAN feature data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanLabelledFeatureData,file=paste0(outputDataDirectory,'/', participantID,'CleanFeature.csv'),row.names=FALSE)
    
    cat(paste0('Writing CLEANFFT data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanLabelledFFTData,file=paste0(outputDataDirectory,'/', participantID,'CleanFFT.csv'),row.names=FALSE)
    
      } else {
    cat(paste0('processed files for ', participantID,' already exist. Loading...','\n'))
        
    labelledData<-list()
    cleanData<-list()
    
    labelledData$instanceLabelData<-fread(file.path(instanceLabelDirectory, paste0(participantID,'ALL.csv')))
    labelledData$labelledFeatureData<-fread(input =paste0(outputDataDirectory,'/', participantID,'ALLFeature.csv'))
    labelledData$labelledFFTData<-fread(input=paste0(outputDataDirectory,'/', participantID,'ALLFFT.csv'))
 
    cleanData$cleanInstanceLabelData<-fread(input = paste0(instanceLabelDirectory,'/', participantID,'Clean.csv'))
    cleanData$cleanLabelledFeatureData<-fread(input=paste0(outputDataDirectory,'/', participantID,'CleanFeature.csv'))
    cleanData$cleanLabelledFFTData<-fread(input=paste0(outputDataDirectory,'/', participantID,'CleanFFT.csv'))
    
      }
  return(list(cleanData=cleanData,labelledData=labelledData))
  
}

#Function for rounding to the nearest 5 seconds, based at the first time entry (ie first time is 00:00:02, all times will be rounded to end with 2 or 7 seconds)
round5secs <- function( x,startTime) { 
  start<-as.POSIXlt(startTime)
  startSecs<-start$sec
  x <- as.POSIXlt( x - startSecs+ as.difftime( 2.5, units="secs" ) )
  x$sec <- 5*(x$sec %/% 5) 
  as.POSIXct(x+startSecs)
} 

checklabelData<-function(checkData, minSeparation=600){
    #Check that every row of the label data has an end time after the start time 
    if(sum(checkData$NewEnd<checkData$NewStart)>0){stop( "some epochs end before they begin!" )}
  
    #Check the sequence of epochs forms a sequence
    nrows<-nrow(checkData)
    if(sum(checkData$NewStart[2:nrows]<checkData$NewEnd[1:nrows-1])>0){stop( "epochs do not form a sequence" )}
  
    #Check that the gaps in between epochs are not too big - default is 10 mins 
    if(sum(checkData$NewStart[2:nrows]-checkData$NewEnd[1:nrows-1]>minSeparation)>0){stop( "there are large gaps between epochs" )}
  
    return(cat(paste0('completed checks for label data from ',checkData$participant[1])))
}


extractLabelsSingleFile = function(all_bouts,identifier, winSize,instanceLabelDirectory) {
  
  dateFmt = '%Y-%m-%d %H:%M:%S'
  tz = 'GMT'
  
  annotations = unique(all_bouts$behavior)
  actNames = sub(" ", "", annotations)
  identifiers = identifier
  for (id in 1:length(identifiers)) {
    cat(identifiers[id], "\n")
    
    bouts = all_bouts[all_bouts$identifier == identifiers[id], ]
    outputFile = file.path(instanceLabelDirectory, paste0(identifiers[id],'ALL.csv'))
    out<-outputFile
    r = 1
    l = 1
    label = "NULL"
    boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt,tz=tz)
    boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt,tz=tz)
    timestamp = boutstart
    
    day = timestamp$mday
    cat(strftime(timestamp, "%Y-%m-%d"), '\n')
    if (!file.exists(instanceLabelDirectory)) {
      dir.create(instanceLabelDirectory, recursive=TRUE)
    }
    if (file.exists(out)) {
      file.remove(out)
    }
    cat("identifier,timestamp,behavior\n", file=out, append=TRUE)
    
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize <= boutstop)) {
        # the window is within this bout - add the label
        if(class(bouts)=="data.frame"){
        label = sub(" ", "", str_trim(bouts[r, c('behavior')]))
        } else if (class(bouts)[1]=="data.table"){
          label = sub(" ", "", str_trim(bouts[r, c('behavior'),with=FALSE]))
        }
      } else if (timestamp + winSize > boutstop) {
        # move on to the next bout
        if (r == nrow(bouts)) {
          break
        }
        while (timestamp + winSize > boutstop) {
          if (r == nrow(bouts)) {
            break
          }
          r = r + 1
          boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt,tz=tz)
          boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt,tz=tz)
        }
        if (timestamp >= boutstart) {
          # the window is within this bout - add the label
          
          if(class(bouts)=="data.frame"){
            label = sub(" ", "", str_trim(bouts[r, c('behavior')]))
          } else if (class(bouts)[1]=="data.table"){
            label = sub(" ", "", str_trim(bouts[r, c('behavior'),with=FALSE]))
          }
        }
      }
      cat(paste0(identifier,','), file=out, append=TRUE)
      cat(strftime(timestamp, "%Y-%m-%d %H:%M:%S,"), file=out, append=TRUE)
      cat(label, file=out, append=TRUE)
      cat("\n", file=out, append=TRUE)
      # next window
      l = l + 1
      label = "NULL"
      timestamp = as.POSIXlt(timestamp + winSize)
    }
  }
  return(actNames)
}


removeUnlabelledEnds<-function(data,unlabelledBehaviorName='unknown'){

kx<-which(!data$instanceLabelData$behavior==unlabelledBehaviorName)
if(length(kx)>0){
  maxIndex<-min(kx[length(kx)],nrow(data$labelledFeatureData))
  
  cleanInstanceLabelData<-data$instanceLabelData[kx[1]:maxIndex,]
  cleanLabelledFeatureData<-data$labelledFeatureData[kx[1]:maxIndex,]
  cleanLabelledFFTData<-data$labelledFFTData[kx[1]:maxIndex,]
}
return(list(cleanInstanceLabelData=cleanInstanceLabelData,cleanLabelledFeatureData=cleanLabelledFeatureData,cleanLabelledFFTData=cleanLabelledFFTData))
}


#Function to compute proximity between two node matricies -  
computeProximity<-function(nodes1,nodes2,parallel=FALSE,mc.cores=1){
  
  #Check that we have the right number of columns
  if(!ncol(nodes1)==ncol(nodes2)){stop( "number of columns do not match!" )}

  ntrees<-ncol(nodes1)
  
  
  P<-matrix(nrow = nrow(nodes1),ncol = nrow(nodes2),dimnames = NULL)
  
  if(parallel==FALSE){
    P <- apply(
      nodes1,
      MARGIN = 1,
      function(a){apply(
        nodes2,
        MARGIN = 1,
        function(b){sum(a==b)})}) 
    return(P/ntrees)
    
  }else if (parallel==TRUE){
    coresPerLevel<-floor(sqrt(mc.cores))
    
    nrows<-nrow(nodes1)
    
    
    #P=sapply(X = 
               
    P=mclapply(
      X=split(nodes2,row(nodes2)),
      FUN=function(a){apply(
        nodes1,
        MARGIN = 1,
        function(b) sum(a==b))
      })
      #,FUN = cbind)
    
    return(matrix(unlist(P), ncol = length(P[[1]]), byrow = TRUE,dimnames = NULL)/ntrees)
 
    
  }
}

#C++ version of computeProximity - faster and better

cppFunction('NumericMatrix computeProximityC(NumericMatrix nodes1, NumericMatrix nodes2) {
  int n1 = nodes1.nrow(), ntrees = nodes1.ncol(), n2= nodes2.nrow();
            
            NumericMatrix proximity(n2,n1);
            
            
            for (int i = 0; i < n1; i++) {
              for (int j = 0; j < n2; j++) {
                for (int k=0; k<ntrees; k++){
                  if(nodes1(i,k)==nodes2(j,k)){
                    proximity(j,i) += 1.0/ntrees;
                  }
            
                }
              }
            }
        
return proximity;
            
}')


#C++ int version of computeProximity - faster and better

cppFunction(code = 'NumericMatrix computeProximityC_int(NumericMatrix nodes1, NumericMatrix nodes2) {
      int n1 = nodes1.nrow(), ntrees = nodes1.ncol(), n2= nodes2.nrow();
            
            NumericMatrix proximity(n2,n1);
            
            
            for (int i = 0; i < n1; i++) {
              
                for (int j = 0; j < n2; j++) {
            
                    for (int k=0; k<ntrees; k++){
            
                        if(nodes1(i,k)==nodes2(j,k)){
                        proximity(j,i) += 1;
                        }
                    }
                }
            }
            
            return proximity;
            
            }')


#Function to roughly split up matrix into chunks
splitMatrix<-function(data_matrix,nprocs){
  
  #divide up matrix into nchunks=ncores chunks
  chunks<-splitNumber(nrow(data_matrix),nprocs)
  
  #attach rownames
  rownames(data_matrix)<-1:nrow(data_matrix)
  
  end_indices<-cumsum(chunks)
  start_indices<-cumsum(c(1,chunks))
  
chunked_matrix<-lapply(X = 1:nprocs,FUN = function(x) data_matrix[start_indices[x]:end_indices[x],])

return(chunked_matrix)
}

#function to divide a number into chunks
splitNumber<-function(number,nprocs){
  
  remainder_chunk<-number %% nprocs
  
  chunk<-number %/% nprocs
  
  if(!remainder_chunk==0){
    large_chunk<-chunk+1
    
    chunk_lengths<-c(rep(large_chunk,remainder_chunk),rep(chunk,nprocs-remainder_chunk))
  } else{
    chunk_lengths<-c(rep(chunk,nprocs))
    nchunks<-nprocs
  }
  return(chunk_lengths)
  }


#Function to roughly split up matrix into chunks
chunkOfMatrix<-function(data_matrix,nchunks,chunkID){
  
  #divide up matrix into nchunks chunks
  chunks<-splitNumber(nrow(data_matrix),nchunks)
  
  #attach rownames
  rownames(data_matrix)<-1:nrow(data_matrix)
  
  end_indices<-cumsum(chunks)
  start_indices<-cumsum(c(1,chunks))
  
  if(chunkID<=nchunks){
  chunk_of_matrix<-data_matrix[start_indices[chunkID]:end_indices[chunkID],]
  
  return(chunk_of_matrix)
  } else {
    cat('chunkID must be smaller than or equal to nchunks')
  }
  
}


calcZ<-function(ProxTrain,Kmax,CV=TRUE){
#Diag <- diag(apply(ProxTrain, 1, sum))
#U<-Diag-ProxTrain
  k   <- Kmax
  
if(CV==TRUE){
ProxTrain<-computeCVmatrix(ProxTrain)
}
  
evL <- eigs_sym(ProxTrain,k+1,which='LM')
  
Z   <- evL$vectors[,1:k]
  
return(Z)
}

kSpaceAnalysis<-function(kval,Z,ProxTest,ProxTrain,TrainingData,testing_RF_predicitions){
  

  #Z is our projection operator
  
  #plot(Z, col=as.factor(AllInstanceData$behavior[ix]), pch=20)
  
  Z_k<-Z[,1:kval]
  #Now project our testing data into K space
  
  kData<-ProxTest %*% Z_k
  
  kTrainData<-ProxTrain %*% Z_k
  
  #Intermediate step:
  #compare the out-of-sample classification performance of LDA trained on {labels,kTrainData}
  #to the original RF trained on {labels, featuredata}, and as a function of k
  #using a random half of the data, 20 times
  
  cat(paste0('doing LDA \n'))
  
  LDA_accuracy<-c()
  kMeans_accuracy<-c()
  
  cat(paste0(ncol(kTrainData), '\n'))
  
  lda_comparison<-lda(x=kTrainData,grouping = as.factor(TrainingData[,1]))
  
  lda_prediction<-predict(object = lda_comparison,newdata=kData,dimen = kval)
  
  reference<-factor(testing_RF_predicitions,levels = levels(lda_prediction$class))
  
  for(i in 1:20){
    
  #take a half subset of data for confusion matrix
    
  ix<-sample(length(lda_prediction$class),replace=F,size=floor(0.5*length(lda_prediction$class)))
 
  
  lda_RF_confusion_matrix<-confusionMatrix(data =lda_prediction$class[ix],reference = reference[ix])
  
  LDA_accuracy[i]<-lda_RF_confusion_matrix$overall[1]
  }
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
  
  LDAperformance<-LDA_accuracy
  # HMMperformance[[i]]<-HMM_confusion_matrix
  
  #write predicitions
  cat(paste0('saving predictions \n'))
  
  
#  write.csv(x=lda_prediction,file = file.path(RFoutput,paste0(outputPrefix,kval,'UCI_LDApred.csv')))
#  write.csv(x=newLabels,file = file.path(HMMoutput,'HMMpred.csv'))
#  write.csv(x=LDAperformance,file = file.path(RFoutput,paste0(outputPrefix,kval,'UCI_LDAaccuracy.csv')))
  
  
  #save(LDAperformance, HMMperformance, file = file.path(resultsDataDirectory,"Results.RData"))
  
  return(LDAperformance)
  
}

computeCVmatrix<-function(Proximity){
  rowsum<-rowSums(Proximity)
  rowsum<-rowsum/ncol(Proximity)
  colsum<-colSums(Proximity)
  colsum<-colsum/nrow(Proximity)
cv=0.5*t(t(Proximity-rowsum+mean(Proximity))-colsum)
return(cv) 
}

computeCVbigmatrix<-function(Proximity,BackingDir){

  rowmean<-BigRowSums(pBigMat = Proximity@address)
  
  matmean<-sum(rowmean)
    
  rowmean<-rowmean/length(rowmean)
  
  rowmean.bigmat<-big.matrix(nrow = nrow(Proximity),ncol = ncol(Proximity),type = 'double',backingfile =BackingDir)
  
  
  colmean<-colsum(Proximity)
  colmean<-rowmean/length(colmean)
  
  matmean<-matmean/(length(colmean)*length(rowmean))
  
  cv=0.5*big.t(big.t(Proximity-rowmean+matmean,dir =BackingDir)-colmean,dir = BackingDir)
  return(cv) 
}


RFProxLDA<-function(TrainingData,TestingData,Kmax=40,ncores,ntree){

  
  #1.a Run RF using the labelled data points on training data

mtry = floor(sqrt(ncol(TrainingData[,2:ncol(TrainingData)])))
replace=TRUE
nodesize=1

cat(paste0('training RF\n'))

cat(paste0('nrow for training data is ',nrow(TrainingData),'\n'))
cat(paste0('size of features for training data is ',object.size(TrainingData),'\n'))

rf <- foreach(ntree=splitNumber(ntree,nprocs = ncores ), .combine=randomForest::combine, .multicombine=TRUE, .packages='randomForest') %dopar%
  randomForest(x = TrainingData[,2:ncol(TrainingData)],y=as.factor(TrainingData[,1]),
               ntree=ntree,
               mtry=mtry,
               replace=replace,
               nodesize=nodesize,
               importance=FALSE,
               proximity = FALSE,
               do.trace = 100)


#1.b we need to know which data point goes to which node of each tree in our training set!
cat(paste0('extracting training nodes \n'))

training_nodes <- foreach(features=splitMatrix(TrainingData[,2:ncol(TrainingData)],nprocs = ncores),.combine = rbind, .packages='randomForest') %dopar% 
  attr(predict(rf, features, type="prob",
               norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=TRUE,oob.prox=TRUE),which='nodes')

training_nodes<-training_nodes[order(as.numeric(rownames(training_nodes))),]


#Challenge - create proximity matrix from node locations

#training_nodes matrix has npoints rows and ntree columns

#2.Output the RF proximity matrix, for all testing data training data - ie for one participant

#Proximity matrix is npoints by npoints
cat(paste0('calculating training nodes proximity \n'))

ProxTrain <- foreach(splitTraining=splitMatrix(training_nodes,nprocs = ncores), .combine = rbind) %dopar% matrix(
  computeProximityC(nodes1=training_nodes,nodes2=splitTraining),
  nrow=nrow(splitTraining),dimnames=list(rownames(splitTraining)))

ProxTrain<-ProxTrain[order(as.numeric(rownames(ProxTrain))),]


#2.b we need to know which data point goes to which node of each tree in our testing set!

cat(paste0('extracting testing nodes \n'))




testing_nodes <- foreach(features=splitMatrix(TestingData,nprocs = ncores),.combine = rbind, .packages='randomForest') %dopar% attr(
  predict(rf, features, type="prob",norm.votes=TRUE, predict.all=TRUE,
          proximity=FALSE, nodes=TRUE),which='nodes')

reordering_indices<-order(as.numeric(row.names(testing_nodes)))
testing_nodes<-testing_nodes[reordering_indices,]

cat(paste0('extracting RF test predictions \n'))

#and also the RF predicitions
testing_RF_predicitions<-foreach(features=splitMatrix(TestingData[,2:ncol(TestingData)],nprocs = ncores),.combine = c, .packages='randomForest') %dopar% 
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


Z<-calcZ(ProxTrain=ProxTrain,Kmax=Kmax,CV=FALSE)
Z_cv<-calcZ(ProxTrain=ProxTrain,Kmax=Kmax,CV = TRUE)


#save(Z,file = file.path(resultsDataDirectory,paste0("UCI_Z.RData")))
#save(Z_cv,file = file.path(resultsDataDirectory,paste0("UCI_Z_cv.RData")))


#load(file='~/Documents/Oxford/Activity/UCI_Z.RData')
#load(file='~/Documents/Oxford/Activity/UCI_Z_cv.RData')

LDAperformance_output<-foreach(k=1:Kmax,.combine = rbind) %dopar% kSpaceAnalysis(
  kval=k,Z=Z,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
  testing_RF_predicitions=testing_RF_predicitions)

LDAperformance_cv_output<-foreach(k=1:Kmax,.combine = rbind) %dopar% kSpaceAnalysis(
  kval=k,Z=Z_cv,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
  testing_RF_predicitions=testing_RF_predicitions)

return(list(LDAperformance_output=LDAperformance_output,LDAperformance_cv_output=LDAperformance_cv_output,Z=Z,Z_cv=Z_cv,ProxTrain=ProxTrain,ProxTest=ProxTest,testing_RF_predicitions=testing_RF_predicitions))

}



plotKData<-function(Proxdatamatrix,Zmat,k,xaxis='X1',yaxis='X2',labelData,name,outputdir){
  kPlotData<-Proxdatamatrix %*% Zmat[,1:k]
  kPlotData<-data.frame(kPlotData)
  kPlotData$group<-factor(labelData)
  kPlotDatamelt <- melt(kPlotData, id.vars = "group")
  P<-ggplot(kPlotData, aes_string(x=xaxis,y=yaxis))+ geom_point(aes(colour = group), size = 1)+ggtitle(gsub(pattern = '.png',replacement = '',x = name))
  ggsave(plot = P,filename =file.path(outputdir,name),device = 'png')
}


plot3DkData<-function(datalist,name,groundtruth,outputdir){
  kSpaceData<-datalist$ProxTrain %*% datalist$Z_cv[,1:3]
  colorVector<-rep('',times=nrow(kSpaceData))
  ia<-which(factor(groundtruth)==levels(factor(groundtruth))[1])
  colorVector[ia]<-'red'
  colorVector[-ia]<-'blue'
  png(filename =file.path(outputdir,name),width = 1000,height = 1000)
  scatterplot3d(kSpaceData, main=gsub(pattern = '3D.png',replacement = '',x = name),color =colorVector, pch = 19)
  dev.off()
}




RF_nodes_chunk<-function(TrainingFData,TrainingBData,TestingFData,ncores,ntree,savefileloc,chunkID,nametoken){
  

  #1.a Run RF using the labelled data points on training data
  
  mtry = floor(sqrt(ncol(TrainingFData)-1))
  replace=TRUE
  nodesize=1
  
  cat(paste0('training RF\n'))
  
  cat(paste0('nrow for training data is ',nrow(TrainingFData),'\n'))
  cat(paste0('size of features for training data is ',object.size(TrainingFData),'\n'))
  
  rf <- foreach(ntree=splitNumber(ntree,nprocs = ncores ), .combine=randomForest::combine, .multicombine=TRUE, .packages='randomForest') %dopar%
    randomForest(x = TrainingFData,y=as.factor(TrainingBData),
                 ntree=ntree,
                 mtry=mtry,
                 replace=replace,
                 nodesize=nodesize,
                 importance=FALSE,
                 proximity = FALSE,
                 do.trace = 100)
  
  
  cat(paste0('saving RF file \n'))
  
  save(rf,file=file.path(savefileloc,paste0('RF_',chunkID,'_',nametoken,'.RData')))
  
  split_Testing_features<-splitMatrix(TestingFData,nprocs = ncores)
  split_Training_features<-splitMatrix(TrainingFData,nprocs = ncores)
  
  rm(TestingFData)
  rm(TrainingFData)
  
  #2.a we need to know which data point goes to which node of each tree in our training set!
  cat(paste0('extracting training nodes \n'))
  
  training_nodes <- foreach(features=split_Training_features,.combine = rbind, .packages='randomForest') %dopar% 
    attr(predict(rf, features, type="prob",
                 norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=TRUE,oob.prox=TRUE),which='nodes')
  
  training_nodes<-training_nodes[order(as.numeric(rownames(training_nodes))),]
  
  cat(paste0('saving training nodes \n'))
  
  write.csv(x = training_nodes,file=file.path(savefileloc,paste0('training_nodes_',chunkID,'_',nametoken,'.csv')),row.names = FALSE)

  rm(training_nodes)
  cat(paste0('extracting testing nodes \n'))
  
  #2.b we need to know which data point goes to which node of each tree in our testing set!
  testing_nodes <- foreach(features=split_Testing_features,.combine = rbind, .packages='randomForest') %dopar% attr(
    predict(rf, features, type="prob",norm.votes=TRUE, predict.all=TRUE,
            proximity=FALSE, nodes=TRUE),which='nodes')
  
  reordering_indices<-order(as.numeric(row.names(testing_nodes)))
  testing_nodes<-testing_nodes[reordering_indices,]
  
  cat(paste0('saving testing nodes \n'))
  
  write.csv(x = testing_nodes,file=file.path(savefileloc,paste0('testing_nodes_',chunkID,'_',nametoken,'.csv')),row.names = FALSE)
  rm(testing_nodes)
  
  splitMatrix(TestingFData,nprocs = ncores)
  
  cat(paste0('extracting RF predictions \n'))
  #3. and also the RF predicitions for the testing set
  testing_RF_predicitions<-foreach(features=split_Testing_features,.combine = c, .packages='randomForest') %dopar%
    as.character(predict(rf, features, type="response",
                         norm.votes=TRUE, predict.all=TRUE, proximity=FALSE, nodes=FALSE)$aggregate,names)


  testing_RF_predicitions<-(testing_RF_predicitions[reordering_indices])


  cat(paste0('saving RF predictions \n'))

  write.csv(x = testing_RF_predicitions,file=file.path(savefileloc,paste0('testing_RF_predicitons_',chunkID,'_',nametoken,'.csv')),row.names = FALSE)
  rm(testing_RF_predicitions)
  
         
  return(cat(paste0('saved files for chunk ',chunkID,' with ',ntree,' trees')))
}



cbind_node_files<-function(inputDirectory,outputDirectory,startToken,leftOutParticipant=participants[leave_out],nchunks){
  
  chunkids<-1:nchunks
  listOfNodeFiles<-paste0(startToken,chunkids,'_',leftOutParticipant,'.csv')

  
  all_nodes<-foreach(file=listOfNodeFiles,.combine = cbind,.multicombine = TRUE) %dopar% fread(input=file.path(outputDirectory,file))
}

rbind_prox_files<-function(inputDirectory,outputDirectory,startToken,leftOutParticipant=participants[leave_out],nchunks){
  
  chunkids<-1:nchunks
  listOfFiles<-paste0(startToken,chunkids,'_',leftOutParticipant,'.csv')
  
  
  all_nodes<-foreach(file=listOfFiles,.combine = rbind,.multicombine = TRUE) %do% fread(input=file.path(outputDirectory,file))
}

#Code to creat rowsum function for bigdata matrix
sourceCpp(code = '// [[Rcpp::depends(BH)]]
#include <Rcpp.h>
          using namespace Rcpp;
          
          // [[Rcpp::depends(BH, bigmemory)]]
          #include <bigmemory/MatrixAccessor.hpp>
          
          #include <numeric>
          
          // Logic for BigRowSums.
          template <typename T>
          NumericVector BigRowSums(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat) {
          NumericVector rowSums(pMat->nrow(), 0.0);
          NumericVector value(1);
          for (int jj = 0; jj < pMat->ncol(); jj++) {
          for (int ii = 0; ii < pMat->nrow(); ii++) {
          value = mat[jj][ii];
          if (all(!is_na(value))) {
          rowSums[ii] += value[0];
          }   
          }   
          }   
          return rowSums;
          }
          
          // Dispatch function for BigRowSums
          //
          // [[Rcpp::export]]
          NumericVector BigRowSums(SEXP pBigMat) {
          XPtr<BigMatrix> xpMat(pBigMat);
          
          switch(xpMat->matrix_type()) {
          case 1:
          return BigRowSums(xpMat, MatrixAccessor<char>(*xpMat));
          case 2:
          return BigRowSums(xpMat, MatrixAccessor<short>(*xpMat));
          case 4:
          return BigRowSums(xpMat, MatrixAccessor<int>(*xpMat));
          case 6:
          return BigRowSums(xpMat, MatrixAccessor<float>(*xpMat));
          case 8:
          return BigRowSums(xpMat, MatrixAccessor<double>(*xpMat));
          default:
          throw Rcpp::exception("unknown type detected for big.matrix object!");
          }   
          }
          ')
  

# sourceCpp(code = '// [[Rcpp::depends(BH)]]
# #include <Rcpp.h>
#           using namespace Rcpp;
#           
#           // [[Rcpp::depends(BH, bigmemory)]]
#           #include <bigmemory/MatrixAccessor.hpp>
#           
#           #include <numeric>
#           
#           // Logic for BigCV
#           template <typename T>
#           NumericVector BigCV(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat,XPtr<BigMatrix> output, MatrixAccessor<T> omat,SEXP rmeans,SEXP cmeans, NumericVector totmean) {
#           NumericVector value(1);
# 
#           for (int jj = 0; jj < pMat->ncol(); jj++) {
#           for (int ii = 0; ii < pMat->nrow(); ii++) {
#           value = mat[jj][ii];
#           omat[jj][ii] = 0.5*(value[0] - rmeans[ii] - cmeans[jj] + totmean[0]);
#           }   
#           }
#           return totmean[0];
#           }
#           
#           // Dispatch function for BigCV
#           //
#           // [[Rcpp::export]]
#           NumericVector BigCV(SEXP pBigMat,SEXP outputBigMat, SEXP rowmeanvals, SEXP colmeanvals, SEXP totalmeanval) {
#           XPtr<BigMatrix> xpMat(pBigMat);
#           XPtr<BigMatrix> xoMat(outputBigMat);
# 
#           
#           switch(xpMat->matrix_type()) {
#           case 1:
#           return BigCV(xpMat, MatrixAccessor<char>(*xpMat),xoMat, MatrixAccessor<char>(*xoMat),rowmeanvals,colmeanvals,totalmeanval);
#           case 2:
#           return BigCV(xpMat, MatrixAccessor<short>(*xpMat),xoMat, MatrixAccessor<short>(*xoMat),rowmeanvals,colmeanvals,totalmeanval);
#           case 4:
#           return BigCV(xpMat, MatrixAccessor<int>(*xpMat),xoMat, MatrixAccessor<int>(*xoMat),rowmeanvals,colmeanvals,totalmeanval);
#           case 6:
#           return BigCV(xpMat, MatrixAccessor<float>(*xpMat),xoMat, MatrixAccessor<float>(*xoMat),rowmeanvals,colmeanvals,totalmeanval);
#           case 8:
#           return BigCV(xpMat, MatrixAccessor<double>(*xpMat),xoMat, MatrixAccessor<double>(*xoMat),rowmeanvals,colmeanvals,totalmeanval);
#           default:
#           throw Rcpp::exception("unknown type detected for big.matrix object!");
#           }   
#           }
#           ')
# 
# 
# sourceCpp(code = '// [[Rcpp::depends(BH)]]
# #include <Rcpp.h>
#           using namespace Rcpp;
#           
#           // [[Rcpp::depends(BH, bigmemory)]]
#           #include <bigmemory/MatrixAccessor.hpp>
#           
#           #include <numeric>
#           
#           // Logic for BigCV
#           template <typename T>
#           NumericVector BigCVpart(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat,XPtr<BigMatrix> output, MatrixAccessor<T> omat, double *rmeans,double *cmeans,double *totmean,int *start_row) {
#           NumericVector value(1);
# 
#           
#           int start_index = start_row[0];
# 
#           for (int jj = 0; jj < pMat->ncol(); jj++) {
#           for (int ii = 0; ii < pMat->nrow(); ii++) {
#           value = mat[jj][ii];
#           omat[jj][ii+start_index] = 0.5*(value[0] - rmeans[ii+start_index] - cmeans[jj] + totmean[0]);
#           }   
#           }
#           return rmeans[1];
#           }
#           
#           // Dispatch function for BigCV
#           //
#           // [[Rcpp::export]]
#           NumericVector BigCVpart(SEXP pBigMat,SEXP outputBigMat, SEXP rowmeanvals, SEXP colmeanvals, SEXP totalmeanval,SEXP start_row) {
#           XPtr<BigMatrix> xpMat(pBigMat);
#           XPtr<BigMatrix> xoMat(outputBigMat);
#           double *prm, *pcm, *ptm, *psr;
#           prm = REAL(rowmeanvals);
#           pcm = REAL(colmeanvals);
#           ptm = REAL(totalmeanval);
#           psr = REAL(start_row);
# 
#           switch(xpMat->matrix_type()) {
#           case 1:
#           return BigCVpart(xpMat, MatrixAccessor<char>(*xpMat),xoMat, MatrixAccessor<char>(*xoMat),prm,pcm,ptm,psr);
#           case 2:
#           return BigCVpart(xpMat, MatrixAccessor<short>(*xpMat),xoMat, MatrixAccessor<short>(*xoMat),prm,pcm,ptm,psr);
#           case 4:
#           return BigCVpart(xpMat, MatrixAccessor<int>(*xpMat),xoMat, MatrixAccessor<int>(*xoMat),prm,pcm,ptm,psr);
#           case 6:
#           return BigCVpart(xpMat, MatrixAccessor<float>(*xpMat),xoMat, MatrixAccessor<float>(*xoMat),prm,pcm,ptm,psr);
#           case 8:
#           return BigCVpart(xpMat, MatrixAccessor<double>(*xpMat),xoMat, MatrixAccessor<double>(*xoMat),prm,pcm,ptm,psr);
#           default:
#           throw Rcpp::exception("unknown type detected for big.matrix object!");
#           }   
#           }
#           ')
# 
# 
# 
# sourceCpp(code = '// [[Rcpp::depends(BH)]]
#           #include <Rcpp.h>
#           using namespace Rcpp;
#           
#           // [[Rcpp::depends(BH, bigmemory)]]
#           #include <bigmemory/MatrixAccessor.hpp>
#           
#           #include <numeric>
#           
#           
#           // Dispatch function for BigCV
#           //
#           // [[Rcpp::export]]
#           NumericVector BigCV(SEXP pBigMat,SEXP outputBigMat, SEXP rmeans, SEXP cmeans, SEXP totalmeanval) {
#           
#           XPtr<BigMatrix> xpMat(pBigMat);
#           XPtr<BigMatrix> xoMat(outputBigMat);
#           MatrixAccessor<double> mat(*xpMat);
#           MatrixAccessor<double> omat(*xoMat);
# 
#           Rcpp::NumericVector rmeansvect(rmeans);
#           Rcpp::NumericVector cmeansvect(cmeans);
#           Rcpp::NumericVector totmean(totalmeanval);
#           Rcpp::NumericVector value(1);
#           
#           for (int jj = 0; jj < xpMat->ncol(); jj++) {
#           for (int ii = 0; ii < xpMat->nrow(); ii++) {
#           value = mat[jj][ii];
#           omat[jj][ii] = 0.5*(value[0] - rmeansvect[ii] - cmeansvect[jj] + totmean[0]);
#           }   
#           }
#           return rmeans;
#           }
#           ')

sourceCpp(code = '// [[Rcpp::depends(BH)]]
          #include <Rcpp.h>
          using namespace Rcpp;
          
          // [[Rcpp::depends(BH, bigmemory)]]
          #include <bigmemory/MatrixAccessor.hpp>
          
          #include <numeric>
          
          
          // Dispatch function for BigCV
          //
          // [[Rcpp::export]]
          NumericVector BigCVpart(SEXP pBigMat,SEXP outputBigMat, SEXP rmeans, SEXP cmeans, SEXP totalmeanval,SEXP start_row,SEXP end_row) {
          
          XPtr<BigMatrix> xpMat(pBigMat);
          XPtr<BigMatrix> xoMat(outputBigMat);
          MatrixAccessor<double> mat(*xpMat);
          MatrixAccessor<double> omat(*xoMat);
          
          Rcpp::NumericVector rmeansvect(rmeans);
          Rcpp::NumericVector cmeansvect(cmeans);
          Rcpp::NumericVector totmean(totalmeanval);
          Rcpp::NumericVector value(1);
          Rcpp::IntegerVector srowvect(start_row);
          Rcpp::IntegerVector erowvect(end_row);
          int srow = srowvect[0];
          int erow = erowvect[0];

          for (int jj = 0; jj < xpMat->ncol(); jj++) {
          for (int ii = srow; ii < erow; ii++) {
          value = mat[jj][ii];
          omat[jj][ii] = 0.5*(value[0] - rmeansvect[ii] - cmeansvect[jj] + totmean[0]);
          }   
          }
          return erow;
          }
          ')





sourceCpp(code = '// [[Rcpp::depends(BH)]]
          #include <Rcpp.h>
          using namespace Rcpp;
          
          // [[Rcpp::depends(BH, bigmemory)]]
          #include <bigmemory/MatrixAccessor.hpp>
          
          #include <numeric>
          
          
          // Dispatch function for BigCV
          //
          // [[Rcpp::export]]
  NumericVector BigRowSumsChunk(SEXP pBigMat,SEXP start_row, SEXP end_row) {

          XPtr<BigMatrix> xpMat(pBigMat);
          MatrixAccessor<double> mat(*xpMat);
          NumericVector value(1);
          Rcpp::IntegerVector srowvect(start_row);
          Rcpp::IntegerVector erowvect(end_row);
          int srow = srowvect[0];
          int erow = erowvect[0];
          int diffrow = erow-srow;
          NumericVector rowSums(diffrow, 0.0);

          
    for (int jj = 0; jj < xpMat->ncol(); jj++) {
      for (int ii = 0; ii < diffrow; ii++) {
        value = mat[jj][ii+srow];
        if (all(!is_na(value))) {
          rowSums[ii] += value[0];
        }   
      }   
    }   
    return rowSums;
  }')



sourceCpp(code = '// [[Rcpp::depends(BH)]]
          #include <Rcpp.h>
          using namespace Rcpp;
          
          // [[Rcpp::depends(BH, bigmemory)]]
          #include <bigmemory/MatrixAccessor.hpp>
          
          #include <numeric>
          
          
          // Dispatch function for BigCV
          //
          // [[Rcpp::export]]
          NumericVector BigColSumsChunk(SEXP pBigMat,SEXP start_col, SEXP end_col) {
          
          XPtr<BigMatrix> xpMat(pBigMat);
          MatrixAccessor<double> mat(*xpMat);
          NumericVector value(1);
          Rcpp::IntegerVector scolvect(start_col);
          Rcpp::IntegerVector ecolvect(end_col);
          int scol = scolvect[0];
          int ecol = ecolvect[0];
          int diffcol = ecol-scol;
          NumericVector colSums(diffcol, 0.0);
          
          
          for (int jj = 0; jj < diffcol; jj++) {
          for (int ii = 0; ii < xpMat->nrow(); ii++) {
          value = mat[jj+scol][ii];
          if (all(!is_na(value))) {
          colSums[jj] += value[0];
          }   
          }   
          }   
          return colSums;
          }')



computeCVbigmatrix<-function(Proximity.bigmatrix.descfilepath,CV.bigmatrix.descfilepath,rowmeanvalues,colmeanvalues,meanvalue,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){

#attach proxmatrix
Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)

#attach CV output matrix
CV.bigmatrix<-attach.big.matrix(CV.bigmatrix.descfilepath)


chunkIndices<-splitNumber(number = nrow(Proximity.bigmatrix),nprocs = nchunks)
chunkIndices<-c(0,cumsum(chunkIndices))
#now find indices that we are to calculate CV over for this node
startIndex<-chunkIndices[chunkID]
endIndex<-chunkIndices[chunkID+1]


#now for this core
procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
procIndices<-c(startIndex,startIndex+cumsum(procIndices))

procStartIndex<-procIndices[coreID]
procEndIndex<-procIndices[coreID+1]

endrow<-BigCVpart(pBigMat = Proximity.bigmatrix@address,outputBigMat = CV.bigmatrix@address,rmeans = rowmeanvalues,cmeans =colmeanvalues,totalmeanval =meanvalue,start_row =procStartIndex, end_row = procEndIndex )

return(cat(paste0('finished CV for node ',chunkID,' and core ',coreID,'\n')))

}

ComputeRowSumBigMatrixChunk<-function(Proximity.bigmatrix.descfilepath,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
  
  #attach proxmatrix
  Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)
  
  
  chunkIndices<-splitNumber(number = nrow(Proximity.bigmatrix),nprocs = nchunks)
  chunkIndices<-c(0,cumsum(chunkIndices))
  #now find indices that we are to calculate CV over for this node
  startIndex<-chunkIndices[chunkID]
  endIndex<-chunkIndices[chunkID+1]
  
  
  #now for this core
  procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
  procIndices<-c(startIndex,startIndex+cumsum(procIndices))
  
  procStartIndex<-procIndices[coreID]
  procEndIndex<-procIndices[coreID+1]
  
  rowsums<-BigRowSumsChunk(pBigMat=Proximity.bigmatrix@address,start_row =procStartIndex,end_row= procEndIndex)

  return(rowsums)
}

ComputeColSumBigMatrixChunk<-function(Proximity.bigmatrix.descfilepath,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
  
  #attach proxmatrix
  Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)
  
  
  chunkIndices<-splitNumber(number = ncol(Proximity.bigmatrix),nprocs = nchunks)
  chunkIndices<-c(0,cumsum(chunkIndices))
  #now find indices that we are to calculate CV over for this node
  startIndex<-chunkIndices[chunkID]
  endIndex<-chunkIndices[chunkID+1]
  
  
  #now for this core
  procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
  procIndices<-c(startIndex,startIndex+cumsum(procIndices))
  
  procStartIndex<-procIndices[coreID]
  procEndIndex<-procIndices[coreID+1]
  
  rowsums<-BigColSumsChunk(pBigMat=Proximity.bigmatrix@address,start_col =procStartIndex,end_col= procEndIndex)
  
  return(rowsums)
}



  
  
  