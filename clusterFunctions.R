  #Function for cleaning up raw bout data to machine-readible format for analysis
#cleanData <- function(jointFiles=jointFiles,labelDirectory=labelDirectory,dataDirectory=dataDirectory,outputLabelDir=outputLabelDir,instanceLabelDirectory=instanceLabelDirectory){

cleanData <- function(jointFiles,labelDirectory,dataDirectory,outputLabelDir,instanceLabelDirectory,outputDataDirectory,onlyLoad=FALSE,FFT=FALSE,duration=30){
  
  
  participantID<-gsub(jointFiles[3],pattern = ".csv",replacement = '')
  cat(paste0('starting data processing/loading for ', participantID,'\n'))
  
  if((file.exists(file.path(instanceLabelDirectory,paste0(participantID,'Clean.csv')))==FALSE) | (onlyLoad==FALSE)){
    
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
    featureData<-read.csv(file = file.path(dataDirectory,jointFiles[2]))
    featureData$identifier<-participantID
    
    #delete first data point
    featureData<-featureData[-1,]
    
    #clean up correlation values
    featureData<-cleanUpCorr(featureData=featureData)
    
    if(FFT){
    #Load up FFT Data
    FFTData<-fread(file.path(dataDirectory,jointFiles[3]))
    FFTData$identifier<-participantID
    FFTData$Time<-substr(FFTData$Time,1,19)
    
    }
    
    
    #remove fractional seconds
    featureData$timestamp<-substr(featureData$timestamp,1,19)

    
    start_times<-labelData$StartDateTime[1]+0:(duration-1)
    
    end_times<-labelData$EndDateTime[nrows]-0:(duration-1)
    
    start_row<-pmatch(start_times,featureData$timestamp,duplicates.ok = TRUE)
    iStart<-which(!is.na(start_row))
    start_row<-start_row[iStart]
    labelData$StartDateTime[1]<-start_times[iStart]
    
    end_row<-pmatch(end_times,substr(featureData$timestamp,1,19),duplicates.ok = TRUE)
    iEnd<-which(!is.na(end_row))
    end_row<-end_row[iEnd]
    labelData$EndDateTime[nrows]<-end_times[iEnd]
    
    labelledData<-list()
    
    #only store data points inside labelled epoch
    
    if(FFT){
    labelledData$labelledFFTData<-FFTData[start_row:(end_row-1),]
    }
    
    #now round labels to align with spacing of features
    labelData$StartDateTime<-roundXsecs(labelData$StartDateTime,startTime = labelData$StartDateTime[1],duration = duration)
    labelData$EndDateTime<-roundXsecs(labelData$EndDateTime,startTime = labelData$StartDateTime[1],duration = duration)
    
    
    #now we need to turn from bout level labels to annotations. Here we use a slightly modified TLBC function, annotationsToLabels
    extractLabelsSingleFile(labelData, identifier= participantID,winSize = duration,instanceLabelDirectory = instanceLabelDirectory)
    
    labelledData$instanceLabelData<-fread(file.path(instanceLabelDirectory, paste0(participantID,'ALL.csv')))
    labelledData$instanceLabelData$identifier<-participantID
    
    #sometimes have to delete last feature data point  as extractLabelsSingleFile sometimes does not output final data point for labels
    if(nrow(labelledData$instanceLabelData)==(end_row-start_row)){
    labelledData$labelledFeatureData<-featureData[start_row:(end_row-1),]
    } else if (nrow(labelledData$instanceLabelData)==(end_row-start_row+1)){
    labelledData$labelledFeatureData<-featureData[start_row:end_row,]
    } else {
      cat('error - off by one error between labels and features!')
    }
    
    
    cat(paste0('Writing ALL feature data file ', participantID,'.csv','\n'))
    #write.csv(x=labelledData$labelledFeatureData,file=paste0(outputDataDirectory,'/', participantID,'ALLFeature.csv'),row.names=FALSE)
    write.csv(x=labelData,file=paste0(outputDataDirectory,'/', participantID,'boutData'),row.names=FALSE)
  
  }
  return(list(cleanData=cleanData,labelledData=labelledData))
  
}

    if(FFT){
    cat(paste0('Writing ALL FFT data file ', participantID,'.csv','\n'))
    write.csv(x=labelledData$labelledFFTData,file=paste0(outputDataDirectory,'/', participantID,'ALLFFT.csv'),row.names=FALSE)
    }
    
    #remove unknown labels at beginning and end of period - this is the 'clean' data
    cleanData<-removeUnlabelledEnds(data=labelledData,FFT=FFT)
   
    cat(paste0('Writing CLEAN instance data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanInstanceLabelData,file=paste0(instanceLabelDirectory,'/', participantID,'Clean.csv'),row.names=FALSE)
    
    cat(paste0('Writing CLEAN feature data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanLabelledFeatureData,file=paste0(outputDataDirectory,'/', participantID,'CleanFeature.csv'),row.names=FALSE)
    if(FFT){
    cat(paste0('Writing CLEANFFT data file ', participantID,'.csv','\n'))
    write.csv(x=cleanData$cleanLabelledFFTData,file=paste0(outputDataDirectory,'/', participantID,'CleanFFT.csv'),row.names=FALSE)
    }
      } else {
    cat(paste0('processed files for ', participantID,' already exist. Loading...','\n'))
        
    labelledData<-list()
    cleanData<-list()
    
    labelledData$instanceLabelData<-fread(file.path(instanceLabelDirectory, paste0(participantID,'ALL.csv')))
    labelledData$labelledFeatureData<-fread(input =paste0(outputDataDirectory,'/', participantID,'ALLFeature.csv'))
    if(FFT){
    labelledData$labelledFFTData<-fread(input=paste0(outputDataDirectory,'/', participantID,'ALLFFT.csv'))
    }
    cleanData$cleanInstanceLabelData<-fread(input = paste0(instanceLabelDirectory,'/', participantID,'Clean.csv'))
    cleanData$cleanLabelledFeatureData<-fread(input=paste0(outputDataDirectory,'/', participantID,'CleanFeature.csv'))
    if(FFT){
    cleanData$cleanLabelledFFTData<-fread(input=paste0(outputDataDirectory,'/', participantID,'CleanFFT.csv'))
    }
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

roundXsecs <- function( x,startTime,duration=30) { 
  start<-as.POSIXlt(startTime)
  startSecs<-start$sec
  x <- as.POSIXlt( x - startSecs+ as.difftime( (duration/2), units="secs" ) )
  x$sec <- duration*(x$sec %/% duration) 
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


removeUnlabelledEnds<-function(data,unlabelledBehaviorName='unknown',FFT=FALSE){

kx<-which(!data$instanceLabelData$behavior==unlabelledBehaviorName)
if(length(kx)>0){
  maxIndex<-min(kx[length(kx)],nrow(data$labelledFeatureData))
  
  cleanInstanceLabelData<-data$instanceLabelData[kx[1]:maxIndex,]
  cleanLabelledFeatureData<-data$labelledFeatureData[kx[1]:maxIndex,]
  if(FFT){
  cleanLabelledFFTData<-data$labelledFFTData[kx[1]:maxIndex,]
  }
}
if(FFT){
return(list(cleanInstanceLabelData=cleanInstanceLabelData,cleanLabelledFeatureData=cleanLabelledFeatureData,cleanLabelledFFTData=cleanLabelledFFTData))
} else {
return(list(cleanInstanceLabelData=cleanInstanceLabelData,cleanLabelledFeatureData=cleanLabelledFeatureData))
}
}

#function to clean up correlation colums that contain � symbol
cleanUpCorr<-function(featureData){
  ixy<-which(featureData$corrxy=='�')
  ixz<-which(featureData$corrxz=='�')
  iyz<-which(featureData$corryz=='�')
  
  #replace with interpolated values
  if(length(ixy>0)){
    featureData$corrxy[ixy]<-as.character(0.5*(as.numeric(featureData$corrxy[ixy+1])+as.numeric(featureData$corrxy[ixy-1])))
    featureData$corrxy<-as.numeric(as.character(featureData$corrxy))
  }
  if(length(ixz>0)){
    featureData$corrxz[ixz]<-as.character(0.5*(as.numeric(featureData$corrxz[ixz+1])+as.numeric(featureData$corrxz[ixz-1])))
    featureData$corrxz<-as.numeric(as.character(featureData$corrxz))
    
  }
  if(length(iyz>0)){
    featureData$corryz[iyz]<-as.character(0.5*(as.numeric(featureData$corryz[iyz+1])+as.numeric(featureData$corryz[iyz-1])))
    featureData$corryz<-as.numeric(as.character(featureData$corryz))
  }
  
  
 return(featureData)   
}

FixNAs<-function(data){
  NArows<-which(rowSums(is.na(data))>0)
  
  for(i in 1:length(NArows)){
    
    NAcols<-which(is.na(AllData[NArows[i],]))
    
      for(j in 1:length(NAcols)){
        #We will search for the nearest entry 99 places up or down that isnt NA and copy that into the value
        downsequence<-(NArows[i]+1):(NArows[i]+100)
        upsequence<-(NArows[i]-1):(NArows[i]-100)
        rowsequence<-c(rbind(downsequence, upsequence))
        potentialreplacements<-which(!is.na(data[rowsequence,NAcols[j]]))
        replacement<-min(potentialreplacements)
        data[NArows[i],NAcols[j]]<-data[rowsequence[potentialreplacements[replacement]],NAcols[j]]
    }
  }
  return(data)
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

splitMatrix_cols<-function(data_matrix,nprocs){
  
  #divide up matrix into nchunks=ncores chunks
  chunks<-splitNumber(ncol(data_matrix),nprocs)
  
  #attach rownames
  colnames(data_matrix)<-1:ncol(data_matrix)
  
  end_indices<-cumsum(chunks)
  start_indices<-cumsum(c(1,chunks))
  
  chunked_matrix<-lapply(X = 1:nprocs,FUN = function(x) data_matrix[,start_indices[x]:end_indices[x]])
  
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


#Function to roughly split up matrix into chunks
chunkOfMatrix_cols<-function(data_matrix,nchunks,chunkID){
  
  #divide up matrix into nchunks chunks
  chunks<-splitNumber(ncol(data_matrix),nchunks)
  
  #attach rownames
  colnames(data_matrix)<-1:ncol(data_matrix)
  
  end_indices<-cumsum(chunks)
  start_indices<-cumsum(c(1,chunks))
  
  if(chunkID<=nchunks){
    chunk_of_matrix<-data_matrix[,start_indices[chunkID]:end_indices[chunkID]]
    
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
  
  LDA_confusion<-matrix(ncol=length(levels(lda_prediction$class)),nrow=length(levels(lda_prediction$class)))
  
  for(i in 1:1){
    
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
  
  LDA_accuracy_mean<-mean(LDA_accuracy)
  LDA_accuracy_std<-sd(LDA_accuracy)
  
  LDAperformance<-list(LDA_conf=lda_RF_confusion_matrix$table)
  #LDAperformance<-LDA_accuracy
  
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

# LDAperformance_output<-foreach(k=1:Kmax,.combine = list) %dopar% kSpaceAnalysis(
#   kval=k,Z=Z,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
#   testing_RF_predicitions=testing_RF_predicitions)

LDAperformance_cv_output<-foreach(k=1:Kmax,.combine = list) %dopar% kSpaceAnalysis(
  kval=k,Z=Z_cv,ProxTest=ProxTest,ProxTrain=ProxTrain,TrainingData=TrainingData,
  testing_RF_predicitions=testing_RF_predicitions)

return(list(LDAperformance_cv_output=LDAperformance_cv_output,Z=Z,Z_cv=Z_cv,ProxTrain=ProxTrain,ProxTest=ProxTest,testing_RF_predicitions=testing_RF_predicitions))

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
  
  rm(TrainingFData)
  rm(TestingFData)
  
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
# 
# #Code to creat rowsum function for bigdata matrix
# sourceCpp(code = '// [[Rcpp::depends(BH)]]
# #include <Rcpp.h>
#           using namespace Rcpp;
#           
#           // [[Rcpp::depends(BH, bigmemory)]]
#           #include <bigmemory/MatrixAccessor.hpp>
#           
#           #include <numeric>
#           
#           // Logic for BigRowSums.
#           template <typename T>
#           NumericVector BigRowSums(XPtr<BigMatrix> pMat, MatrixAccessor<T> mat) {
#           NumericVector rowSums(pMat->nrow(), 0.0);
#           NumericVector value(1);
#           for (int jj = 0; jj < pMat->ncol(); jj++) {
#           for (int ii = 0; ii < pMat->nrow(); ii++) {
#           value = mat[jj][ii];
#           if (all(!is_na(value))) {
#           rowSums[ii] += value[0];
#           }   
#           }   
#           }   
#           return rowSums;
#           }
#           
#           // Dispatch function for BigRowSums
#           //
#           // [[Rcpp::export]]
#           NumericVector BigRowSums(SEXP pBigMat) {
#           XPtr<BigMatrix> xpMat(pBigMat);
#           
#           switch(xpMat->matrix_type()) {
#           case 1:
#           return BigRowSums(xpMat, MatrixAccessor<char>(*xpMat));
#           case 2:
#           return BigRowSums(xpMat, MatrixAccessor<short>(*xpMat));
#           case 4:
#           return BigRowSums(xpMat, MatrixAccessor<int>(*xpMat));
#           case 6:
#           return BigRowSums(xpMat, MatrixAccessor<float>(*xpMat));
#           case 8:
#           return BigRowSums(xpMat, MatrixAccessor<double>(*xpMat));
#           default:
#           throw Rcpp::exception("unknown type detected for big.matrix object!");
#           }   
#           }
#           ')
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
#           NumericVector BigCVpart(SEXP pBigMat,SEXP outputBigMat, SEXP rmeans, SEXP cmeans, SEXP totalmeanval,SEXP start_row,SEXP end_row) {
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
#           Rcpp::IntegerVector srowvect(start_row);
#           Rcpp::IntegerVector erowvect(end_row);
#           int srow = srowvect[0];
#           int erow = erowvect[0];
# 
#           for (int jj = 0; jj < xpMat->ncol(); jj++) {
#           for (int ii = srow; ii < erow; ii++) {
#           value = mat[jj][ii];
#           omat[jj][ii] = 0.5*(value[0] - rmeansvect[ii] - cmeansvect[jj] + totmean[0]);
#           }   
#           }
#           return erow;
#           }
#           ')
# 
# 
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
#   NumericVector BigRowSumsChunk(SEXP pBigMat,SEXP start_row, SEXP end_row) {
# 
#           XPtr<BigMatrix> xpMat(pBigMat);
#           MatrixAccessor<double> mat(*xpMat);
#           NumericVector value(1);
#           Rcpp::IntegerVector srowvect(start_row);
#           Rcpp::IntegerVector erowvect(end_row);
#           int srow = srowvect[0];
#           int erow = erowvect[0];
#           int diffrow = erow-srow;
#           NumericVector rowSums(diffrow, 0.0);
# 
#           
#     for (int jj = 0; jj < xpMat->ncol(); jj++) {
#       for (int ii = 0; ii < diffrow; ii++) {
#         value = mat[jj][ii+srow];
#         if (all(!is_na(value))) {
#           rowSums[ii] += value[0];
#         }   
#       }   
#     }   
#     return rowSums;
#   }')
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
#           NumericVector BigColSumsChunk(SEXP pBigMat,SEXP start_col, SEXP end_col) {
#           
#           XPtr<BigMatrix> xpMat(pBigMat);
#           MatrixAccessor<double> mat(*xpMat);
#           NumericVector value(1);
#           Rcpp::IntegerVector scolvect(start_col);
#           Rcpp::IntegerVector ecolvect(end_col);
#           int scol = scolvect[0];
#           int ecol = ecolvect[0];
#           int diffcol = ecol-scol;
#           NumericVector colSums(diffcol, 0.0);
#           
#           
#           for (int jj = 0; jj < diffcol; jj++) {
#           for (int ii = 0; ii < xpMat->nrow(); ii++) {
#           value = mat[jj+scol][ii];
#           if (all(!is_na(value))) {
#           colSums[jj] += value[0];
#           }   
#           }   
#           }   
#           return colSums;
#           }')
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
#           NumericVector RowSumsChunk(NumericMatrix pBigMat,SEXP start_row, SEXP end_row) {
#           
#           NumericVector value(1);
#           Rcpp::IntegerVector srowvect(start_row);
#           Rcpp::IntegerVector erowvect(end_row);
#           int srow = srowvect[0];
#           int erow = erowvect[0];
#           int diffrow = erow-srow;
#           int ncol_iterate = pBigMat.ncol();
#           NumericVector rowSums(diffrow, 0.0);
#           
#           for (int ii = 0; ii < diffrow; ii++) {
#           for (int jj = 0; jj < ncol_iterate; jj++) {
#           value = pBigMat(ii+srow,jj);
#           if (all(!is_na(value))) {
#           rowSums[ii] += value[0];
#           }   
#           }   
#           }   
#           return rowSums;
#           }')
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
#           NumericVector ColSumsChunk(NumericMatrix pBigMat,SEXP start_col, SEXP end_col) {
#           
#           NumericVector value(1);
#           Rcpp::IntegerVector scolvect(start_col);
#           Rcpp::IntegerVector ecolvect(end_col);
#           int scol = scolvect[0];
#           int ecol = ecolvect[0];
#           int diffcol = ecol-scol;
#           int nrow_iterate = pBigMat.nrow();
#           NumericVector colSums(diffcol, 0.0);
#           
#           
#           for (int ii = 0; ii < nrow_iterate; ii++) {
#           for (int jj = 0; jj < diffcol; jj++) {
#           value = pBigMat(ii,jj+scol);
#           if (all(!is_na(value))) {
#           colSums[jj] += value[0];
#           }   
#           }   
#           }   
#           return colSums;
#           }')
# 
# 
# computeCVbigmatrix<-function(Proximity.bigmatrix.descfilepath,CV.bigmatrix.descfilepath,rowmeanvalues,colmeanvalues,meanvalue,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
# 
# #attach proxmatrix
# Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)
# 
# #attach CV output matrix
# CV.bigmatrix<-attach.big.matrix(CV.bigmatrix.descfilepath)
# 
# 
# chunkIndices<-splitNumber(number = nrow(Proximity.bigmatrix),nprocs = nchunks)
# chunkIndices<-c(0,cumsum(chunkIndices))
# #now find indices that we are to calculate CV over for this node
# startIndex<-chunkIndices[chunkID]
# endIndex<-chunkIndices[chunkID+1]
# 
# 
# #now for this core
# procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
# procIndices<-c(startIndex,startIndex+cumsum(procIndices))
# 
# procStartIndex<-procIndices[coreID]
# procEndIndex<-procIndices[coreID+1]
# 
# endrow<-BigCVpart(pBigMat = Proximity.bigmatrix@address,outputBigMat = CV.bigmatrix@address,rmeans = rowmeanvalues,cmeans =colmeanvalues,totalmeanval =meanvalue,start_row =procStartIndex, end_row = procEndIndex )
# 
# return(cat(paste0('finished CV for node ',chunkID,' and core ',coreID,'\n')))
# 
# }
# 
# ComputeRowSumBigMatrixChunk<-function(Proximity.bigmatrix.descfilepath,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
#   
#   #attach proxmatrix
#   Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)
#   
#   
#   chunkIndices<-splitNumber(number = nrow(Proximity.bigmatrix),nprocs = nchunks)
#   chunkIndices<-c(0,cumsum(chunkIndices))
#   #now find indices that we are to calculate CV over for this node
#   startIndex<-chunkIndices[chunkID]
#   endIndex<-chunkIndices[chunkID+1]
#   
#   
#   #now for this core
#   procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
#   procIndices<-c(startIndex,startIndex+cumsum(procIndices))
#   
#   procStartIndex<-procIndices[coreID]
#   procEndIndex<-procIndices[coreID+1]
#   
#   rowsums<-BigRowSumsChunk(pBigMat=Proximity.bigmatrix@address,start_row =procStartIndex,end_row= procEndIndex)
# 
#   return(rowsums)
# }
# 
# ComputeColSumBigMatrixChunk<-function(Proximity.bigmatrix.descfilepath,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
#   
#   #attach proxmatrix
#   Proximity.bigmatrix<-attach.big.matrix(Proximity.bigmatrix.descfilepath)
#   
#   
#   chunkIndices<-splitNumber(number = ncol(Proximity.bigmatrix),nprocs = nchunks)
#   chunkIndices<-c(0,cumsum(chunkIndices))
#   #now find indices that we are to calculate CV over for this node
#   startIndex<-chunkIndices[chunkID]
#   endIndex<-chunkIndices[chunkID+1]
#   
#   
#   #now for this core
#   procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
#   procIndices<-c(startIndex,startIndex+cumsum(procIndices))
#   
#   procStartIndex<-procIndices[coreID]
#   procEndIndex<-procIndices[coreID+1]
#   
#   colsums<-BigColSumsChunk(pBigMat=Proximity.bigmatrix@address,start_col =procStartIndex,end_col= procEndIndex)
#   
#   return(colsums)
# }

bind_sum_files<-function(inputDirectory,startToken,leftOutParticipant=participants[leave_out],start,stop){

  chunkids<-start:stop
  listOfFiles<-paste0(startToken,'_',leftOutParticipant,'_',chunkids,'_subsampled.csv')

  all_nodes<-foreach(fileaddress=listOfFiles,.combine = rbind,.multicombine = TRUE) %dopar% as.vector(read.csv(file=file.path(inputDirectory,fileaddress)))
}


ComputeRowSumMatrixChunk<-function(Proximity_chunk){
  

  rowsums<-RowSumsChunk(pBigMat=Proximity_chunk,start_row =0,end_row= nrow(Proximity_chunk))
  
  return(rowsums)
}

ComputeColSumMatrixChunk<-function(Proximity_chunk){
  

  colsums<-ColSumsChunk(pBigMat=Proximity_chunk,start_col =0,end_col= ncol(Proximity_chunk))
  
  return(colsums)
}
#' 
#' 
#' sourceCpp(code = '// [[Rcpp::depends(BH)]]
#'           #include <Rcpp.h>
#'           using namespace Rcpp;
#'           
#'           // [[Rcpp::depends(BH, bigmemory)]]
#'           #include <bigmemory/MatrixAccessor.hpp>
#'           
#'           #include <numeric>
#'           
#'           
#'           // Dispatch function for BigCV
#'           //
#'           // [[Rcpp::export]]
#'           NumericVector HalfBigCVpart(NumericMatrix pBigMat,SEXP outputBigMat, SEXP rmeans, SEXP cmeans, SEXP totalmeanval,SEXP start_row) {
#'           
#'           XPtr<BigMatrix> xoMat(outputBigMat);
#'           MatrixAccessor<double> omat(*xoMat);
#'           
#'           Rcpp::NumericVector rmeansvect(rmeans);
#'           Rcpp::NumericVector cmeansvect(cmeans);
#'           Rcpp::NumericVector totmean(totalmeanval);
#'           Rcpp::NumericVector value(1);
#'           Rcpp::IntegerVector srowvect(start_row);
#'           int srow = srowvect[0];
#' 
#'           for (int jj = 0; jj < pBigMat.ncol(); jj++) {
#'           for (int ii = 0; ii < pBigMat.nrow(); ii++) {
#' 
#'           value = pBigMat(ii,jj);
#'           omat[jj][ii+srow] = 0.5*(value[0] - rmeansvect[ii] - cmeansvect[jj] + totmean[0]);
#'           }   
#'           }
#'           return 0;
#'           }
#'           ')
#' 
#' 
#' computeCVhalfbigmatrix<-function(Proximity.matrix,CV.bigmatrix.descfilepath,rowmeanvalues,colmeanvalues,meanvalue,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID){
#'   
#' 
#'   #attach CV output matrix
#'   CV.bigmatrix<-attach.big.matrix(CV.bigmatrix.descfilepath)
#'   
#'   
#'   chunkIndices<-splitNumber(number = nrow(CV.bigmatrix),nprocs = nchunks)
#'   chunkIndices<-c(0,cumsum(chunkIndices))
#'   #now find indices that we are to calculate CV over for this node
#'   startIndex<-chunkIndices[chunkID]
#'   endIndex<-chunkIndices[chunkID+1]
#'   
#'   
#'   #now for this core
#'   procIndices<-splitNumber(number = (endIndex-startIndex),nprocs = ncores)
#'   procIndices<-c(startIndex,startIndex+cumsum(procIndices))
#'   
#'   procStartIndex<-procIndices[coreID]
#' 
#'   zero<-HalfBigCVpart(pBigMat = Proximity.matrix,outputBigMat = CV.bigmatrix@address,rmeans = rowmeanvalues,cmeans =colmeanvalues,totalmeanval =meanvalue,start_row =procStartIndex)
#'   return(cat(paste0('finished CV for node ',chunkID,' and core ',coreID,'\n')))
#'   
#' }
#' 
#' #' Find a few approximate largest singular values and corresponding
#' #' singular vectors of a matrix.
#' #'
#' #' The augmented implicitly restarted Lanczos bi-diagonalization algorithm
#' #' (IRLBA) finds a few approximate largest singular values and corresponding
#' #' singular vectors of a sparse or dense matrix using a method of Baglama and
#' #' Reichel.  It is a fast and memory-efficient way to compute a partial SVD.
#' #'
#' #' @param A numeric real- or complex-valued matrix or real-valued sparse matrix.
#' #' @param nv number of right singular vectors to estimate.
#' #' @param nu number of left singular vectors to estimate (defaults to \code{nv}).
#' #' @param maxit maximum number of iterations.
#' #' @param work working subspace dimension, larger values can speed convergence at the cost of more memory use.
#' #' @param reorth if \code{TRUE}, apply full reorthogonalization to both SVD bases, otherwise
#' #'   only apply reorthogonalization to the right SVD basis vectors; the latter case is cheaper per
#' #'   iteration but, overall, may require more iterations for convergence.
#' #' @param tol convergence is determined when \eqn{\|AV - US\| < tol\|A\|}{||AV - US|| < tol*||A||},
#' #'   where the spectral norm ||A|| is approximated by the
#' #'   largest estimated singular value, and U, V, S are the matrices corresponding
#' #'   to the estimated left and right singular vectors, and diagonal matrix of
#' #'   estimated singular values, respectively.
#' #' @param v optional starting vector or output from a previous run of \code{irlba} used
#' #'   to restart the algorithm from where it left off (see the notes).
#' #' @param right_only logical value indicating return only the right singular vectors
#' #'  (\code{TRUE}) or both sets of vectors (\code{FALSE}). The right_only option can be
#' #'  cheaper to compute and use much less memory when \code{nrow(A) >> ncol(A)}.
#' #' @param verbose logical value that when \code{TRUE} prints status messages during the computation.
#' #' @param scale optional column scaling vector whose values divide each column of \code{A};
#' #'   must be as long as the number of columns of \code{A} (see notes).
#' #' @param center optional column centering vector whose values are subtracted from each
#' #'   column of \code{A}; must be as long as the number of columns of \code{A} and may
#' #"   not be used together with the deflation options below (see notes).
#' #' @param du optional subspace deflation vector (see notes).
#' #' @param ds optional subspace deflation scalar (see notes).
#' #' @param dv optional subspace deflation vector (see notes).
#' #' @param shift optional shift value (square matrices only, see notes).
#' #' @param mult optional custom matrix multiplication function (default is \code{\%*\%}, see notes).
#' #' @param fastpath try a fast C algorithm implementation if possible, set \code{fastpath=FALSE} to always use the reference implementation.
#' #'
#' #' @return
#' #' Returns a list with entries:
#' #' \itemize{
#' #'   \item{d}{ max(nu, nv) approximate singular values}
#' #'   \item{u}{ nu approximate left singular vectors (only when right_only=FALSE)}
#' #'   \item{v}{ nv approximate right singular vectors}
#' #'   \item{iter}{ The number of Lanczos iterations carried out}
#' #'   \item{mprod}{ The total number of matrix vector products carried out}
#' #' }
#' #'
#' #' @note
#' #' The syntax of \code{irlba} partially follows \code{svd}, with an important
#' #' exception. The usual R \code{svd} function always returns a complete set of
#' #' singular values, even if the number of singular vectors \code{nu} or \code{nv}
#' #' is set less than the maximum. The \code{irlba} function returns a number of
#' #' estimated singular values equal to the maximum of the number of specified
#' #' singular vectors \code{nu} and \code{nv}.
#' #'
#' #' Use the optional \code{scale} parameter to implicitly scale each column of
#' #' the matrix \code{A} by the values in the \code{scale} vector, computing the
#' #' truncated SVD of the column-scaled \code{sweep(A, 2, scale, FUN=`/`)}, or
#' #' equivalently, \code{A \%*\% diag(1 / scale)}, without explicitly forming the
#' #' scaled matrix. \code{scale} must be a non-zero vector of length equal
#' #' to the number of columns of \code{A}.
#' #'
#' #' Use the optional \code{center} parameter to implicitly subtract the values
#' #' in the \code{center} vector from each column of \code{A}, computing the
#' #' truncated SVD of \code{sweep(A, 2, center, FUN=`-`)},
#' #' without explicitly forming the centered matrix. This option may not be
#' #' used together with the general rank 1 deflation options. \code{center}
#' #' must be a vector of length equal to the number of columns of \code{A}.
#' #' This option may be used to efficiently compute principal components without
#' #' explicitly forming the centered matrix (which can, importantly, preserve
#' #' sparsity in the matrix). See the examples.
#' #'
#' #' Use the optional deflation parameters to compute the rank-one deflated
#' #' SVD of \eqn{A - ds \cdot du dv^T}{A - ds*du \%*\% t(dv)}, where
#' #' \eqn{du^T A - ds\cdot dv^T = 0}{t(du) \%*\% A - ds * t(dv) == 0}. For
#' #' example, the triple \code{ds, du, dv} may be a known singular value
#' #' and corresponding singular vectors. Or \code{ds=m} and \code{dv}
#' #' and \code{du} represent a vector of column means of \code{A} and of ones,
#' #' respectively, where \code{m} is the number of rows of \code{A}.
#' #' This is a rarely used option, but it is used internally
#' #' by the \code{center} option and the two sets of parameters are
#' #' prevented from being used in combination.
#' #'
#' #' Specify an optional alternative matrix multiplication operator in the
#' #' \code{mult} parameter. \code{mult} must be a function of two arguments,
#' #' and must handle both cases where one argument is a vector and the other
#' #' a matrix. See the examples. Special care must be taken when deflation
#' #' is also desired; see the package vignette for details.
#' #'
#' #' Use the \code{v} option to supply a starting vector for the iterative
#' #' method. A random vector is used by default. Optionally set \code{v} to
#' #' the ouput of a previous run of \code{irlba} to restart the method, adding
#' #' additional singular values/vectors without recomputing the already computed
#' #' subspace.
#' #'
#' #'
#' #' @references
#' #' Augmented Implicitly Restarted Lanczos Bidiagonalization Methods, J. Baglama and L. Reichel, SIAM J. Sci. Comput. 2005.
#' #'
#' #' @examples
#' #' set.seed(1)
#' #'
#' #' A <- matrix(runif(200), nrow=20)
#' #' S <- irlba(A)
#' #' S$d
#' #'
#' #' # Compare with svd
#' #' svd(A)$d[1:5]
#' #'
#' #' # Principal components
#' #' P <- irlba(A, nv=1, center=colMeans(A))
#' #'
#' #' # Compare with prcomp (might vary up to sign)
#' #' cbind(P$v, prcomp(A)$rotation[, 1])
#' #'
#' #' # A custom matrix multiplication function that scales the columns of A
#' #' # (cf the scale option). This function scales the columns of A to unit norm.
#' #' col_scale <- sqrt(apply(A, 2, crossprod))
#' #' mult <- function(x, y)
#' #'         {
#' #'           # check if x is a  vector
#' #'           if (is.vector(x))
#' #'           {
#' #'             return((x %*% y) / col_scale)
#' #'           }
#' #'           # else x is the matrix
#' #'           x %*% (y / col_scale)
#' #'         }
#' #' irlba(A, 3, mult=mult)$d
#' #'
#' #' # Compare with:
#' #' irlba(A, 3, scale=col_scale)$d
#' #'
#' #' # Compare with:
#' #' svd(sweep(A, 2, col_scale, FUN=`/`))$d[1:3]
#' #'
#' #' @seealso \code{\link{svd}}, \code{\link{prcomp}}, \code{\link{partial_eigen}}
#' #' @import Matrix
#' #' @importFrom stats rnorm
#' #' @importFrom methods slotNames
#' #' @useDynLib irlba
#' #' @export
#' irlba.big<-
#'   function (A,                     # data matrix
#'             nv=5, nu,              # number of singular vectors to estimate
#'             maxit=1000,            # maximum number of iterations
#'             work=nv + 5,           # working subspace size
#'             reorth=TRUE,           # TRUE=full reorthogonalization
#'             tol=1e-5,              # stopping tolerance
#'             v=NULL,                # optional starting vector or restart
#'             right_only=FALSE,      # TRUE=only return V
#'             verbose=FALSE,         # display status messages
#'             scale,                 # optional column scaling
#'             center,                # optional column centering
#'             du, ds, dv,            # optional general rank 1 deflation
#'             shift,                 # optional shift for square matrices
#'             mult,                  # optional custom matrix multiplication func.
#'             fastpath=TRUE)         # use the faster C implementation if possible
#'   {
#'     # ---------------------------------------------------------------------
#'     # Check input parameters
#'     # ---------------------------------------------------------------------
#'     ropts <- options(warn=1) # immediately show warnings
#'     on.exit(options(ropts))  # reset on exit
#'     eps <- .Machine$double.eps
#'     deflate <- missing(du) + missing(ds) + missing(dv)
#'     if (deflate == 3)
#'     {
#'       deflate <- FALSE
#'     } else if (deflate == 0)
#'     {
#'       deflate <- TRUE
#'       if (length(ds) > 1) stop("deflation limited to one dimension")
#'       if (!is.null(dim(du))) du <- du[, 1]
#'       if (!is.null(dim(dv))) dv <- dv[, 1]
#'     } else stop("all three du ds dv parameters must be specified for deflation")
#'     if (!missing(center))
#'     {
#'       if (deflate) stop("the center parameter can't be specified together with deflation parameters")
#'       if (length(center) != ncol(A)) stop("center must be a vector of length ncol(A)")
#'       du <- rep(1, nrow(A))
#'       ds <- 1
#'       dv <- center
#'       deflate <- TRUE
#'     }
#'     iscomplex <- is.complex(A)
#'     m <- nrow(A)
#'     n <- ncol(A)
#'     if (missing(nu)) nu <- nv
#'     if (!missing(mult) && deflate) stop("the mult parameter can't be specified together with deflation parameters")
#'     missingmult <- FALSE
#'     if (missing(mult))
#'     {
#'       missingmult <- TRUE
#'       mult <- `%*%`
#'     }
#'     k <- max(nu, nv)
#'     k_org <- k;
#'     if (k <= 0)  stop("max(nu, nv) must be positive")
#'     if (k > min(m - 1, n - 1)) stop("max(nu, nv) must be strictly less than min(nrow(A), ncol(A))")
#'     if (k > 0.5 * min(m, n))
#'     {
#'       warning("You're computing a large percentage of total singular values, standard svd might work better!")
#'     }
#'     if (work <= 1) stop("work must be greater than 1")
#'     if (tol < 0) stop("tol must be non-negative")
#'     if (maxit <= 0) stop("maxit must be positive")
#'     if (work <= k) work <- k + 1
#'     if (work >= min(n, m))
#'     {
#'       work <- min(n, m) - 1
#'       if (work <= k)
#'       {
#'         k <- work - 1
#'       }
#'     }
#'     if (tol < eps) tol <- eps
#'     w_dim <- work
#'     if (right_only)
#'     {
#'       w_dim <- 1
#'       work <- min(min(m, n), work + 10 ) # need this to help convergence
#'     }
#'     
#'     # Try to use the fast C code path
#'     if(fastpath && missingmult && is.matrix(A) & !iscomplex && !deflate && missing(scale) && !is.list(v))
#'     {
#'       if (is.null(v))
#'         v <- rnorm(n)
#'       ans <- .Call("IRLB", A, as.integer(k), as.double(v), as.integer(work),
#'                    as.integer(maxit), as.double(tol), .Machine$double.eps, PACKAGE="irlba")
#'       if(ans[[6]] == 0 || ans[[6]] == -2)
#'       {
#'         names(ans) <- c("d", "u", "v", "iter", "mprod", "err")
#'         if(ans[[6]] == -2) warning("did not converge; try increasing maxit or fastpath=FALSE")
#'         return(ans[-6])
#'       }
#'       errors <- c("invalid dimensions",
#'                   "didn't converge",
#'                   "out of memory",
#'                   "starting vector near the null space",
#'                   "linear dependency encountered")
#'       warning("fast code path encountered error ", errors[abs(ans[6])], "; re-trying with fastpath=FALSE.")
#'     }
#'     
#'     # Allocate memory for W and F:
#'     W <- matrix(0.0, m, w_dim)
#'     V <- v
#'     restart <- FALSE
#'     if (is.list(v))
#'     {
#'       if (is.null(v$v) || is.null(v$d) || is.null(v$u)) stop("restart requires left and right singular vectors")
#'       if (max(nu, nv) <= min(ncol(v$u), ncol(v$v))) return(v) # Nothing to do!
#'       right_only <- FALSE
#'       W[, 1:ncol(v$u)] <- v$u
#'       d <- v$d
#'       V <- v$v
#'       restart <- TRUE
#'     }
#'     F <- matrix(0.0, n, 1)
#'     # If starting matrix V is not given then set V to be an (n x 1) matrix of
#'     # normally distributed random numbers.  In any case, allocate V appropriate to
#'     # problem size:
#'     if (is.null(V))
#'     {
#'       V <- matrix(0.0, n, work)
#'       V[, 1] <- rnorm(n)
#'     }
#'     else V <- cbind(V, matrix(0.0, n, work - ncol(V)))
#'     
#'     
#'     # ---------------------------------------------------------------------
#'     # Initialize local variables
#'     # ---------------------------------------------------------------------
#'     B <- NULL                  # Bidiagonal matrix
#'     Bsz <- NULL                # Size of B
#'     eps23 <- eps ^ (2 / 3)         # Used for Smax/avoids using zero
#'     iter <- 1                  # Man loop iteration count
#'     mprod <- 0                 # Number of matrix-vector products
#'     R_F <- NULL                # 2-norm of residual vector F
#'     sqrteps <- sqrt(eps)       #
#'     Smax <- 1                  # Max value of all computed singular values of
#'     # B est. ||A||_2
#'     Smin <- NULL               # Min value of all computed singular values of
#'     # B est. cond(A)
#'     SVTol <- max(sqrteps, tol)  # Tolerance for singular vector convergence
#'     
#'     # Check for user-supplied restart condition
#'     if (restart)
#'     {
#'       B <- cbind(diag(d), 0)
#'       k <- length(d)
#'       
#'       F <- rnorm(n)
#'       F <- orthog(F, V[, 1:k])
#'       V[, k + 1] <- F / norm2(F)
#'     }
#'     
#'     # ---------------------------------------------------------------------
#'     # Main iteration
#'     # ---------------------------------------------------------------------
#'     while (iter <= maxit)
#'     {
#'       # ---------------------------------------------------------------------
#'       # Compute the Lanczos bidiagonal decomposition:
#'       # such that AV  = WB
#'       # and       t(A)W = VB + Ft(E)
#'       # This routine updates W, V, F, B, mprod
#'       #
#'       # Note on scale and center: These options are applied implicitly below
#'       # for maximum computational efficiency. This complicates their application
#'       # somewhat, but saves a few flops.
#'       # ---------------------------------------------------------------------
#'       j <- 1
#'       #   Normalize starting vector:
#'       if (iter == 1 && !restart)
#'       {
#'         V[, 1] <- V[, 1] / norm2(V[, 1])
#'       }
#'       else j <- k + 1
#'       #   j_w is used here to support the right_only=TRUE case.
#'       j_w <- ifelse(w_dim > 1, j, 1)
#'       
#'       #   Compute W=AV
#'       #   Optionally apply scale
#'       VJ <- V[, j]
#'       if (!missing(scale))
#'       {
#'         VJ <- VJ / scale
#'       }
#'       #   Handle sparse products.
#'       avj <- mult(A, VJ)
#'       if("Matrix" %in% attributes(class(avj)) && "x" %in% slotNames(avj))
#'       {
#'         if(length(avj@x) == nrow(W)) avj <- slot(avj, "x")
#'         else avj <- as.vector(avj)
#'       }
#'       W[, j_w] <- avj
#'       
#'       mprod <- mprod + 1
#'       
#'       #   Optionally apply shift
#'       if (!missing(shift))
#'       {
#'         W[, j_w] <- W[, j_w] + shift * VJ
#'       }
#'       
#'       #   Optionally apply deflation
#'       if (deflate)
#'       {
#'         W[, j_w] <- W[, j_w] - ds * cross(dv, VJ) * du
#'       }
#'       
#'       #   Orthogonalize W
#'       if (iter != 1 && w_dim > 1 && reorth)
#'       {
#'         W[, j] <- orthog (W[, j, drop=FALSE], W[, 1:(j - 1), drop=FALSE])
#'       }
#'       
#'       S <- norm2(W[, j_w, drop=FALSE])
#'       #   Check for linearly dependent vectors
#'       if ( (S < SVTol) && (j == 1)) stop("Starting vector near the null space")
#'       if (S < SVTol)
#'       {
#'         W[, j_w] <- rnorm(nrow(W))
#'         if (w_dim > 1) W[, j] <- orthog(W[, j], W[, 1:(j - 1)])
#'         W[, j_w] <- W[, j_w] / norm2(W[, j_w])
#'         S <- 0
#'       }
#'       else W[, j_w] <- W[, j_w] / S
#'       
#'       #   Lanczos process
#'       while (j <= work)
#'       {
#'           j_w <- ifelse(w_dim > 1, j, 1)
#'           
#'           ## workaround for big.matrix problem when handed n x 1 vector,  yields a non-conforming F
#'           
#'           if(class(A)=="big.matrix"){
#'             if (iscomplex)
#'             {
#'               F <- Conj( (as.matrix(mult(Conj(t(W[,j_w,drop=FALSE])), A))))
#'             }
#'             else F <- as.matrix(mult(t(W[,j_w,drop=FALSE]), A))
#'             
#'           } else {
#'             
#'             if (iscomplex)
#'             {
#'               F <- Conj(t(as.matrix(mult(Conj(t(W[,j_w,drop=FALSE])), A))))
#'             }
#'             else F <- t(as.matrix(mult(t(W[,j_w,drop=FALSE]), A)))
#'             
#'           }
#'           
#'         #     Optionally apply shift and scale
#'         if (!missing(shift)) F <- F + shift * W[, j_w]
#'         if (!missing(scale)) F <- F / scale
#'         mprod <- mprod + 1
#'         F <- drop(F - S * V[, j])
#'         #     Orthogonalize
#'         F <- orthog(F, V[, 1:j, drop=FALSE])
#'         if (j + 1 <= work)
#'         {
#'           R <- norm2(F)
#'           #       Check for linear dependence
#'           if (R <= SVTol)
#'           {
#'             F <- matrix(rnorm(dim(V)[1]), dim(V)[1], 1)
#'             F <- orthog(F, V[, 1:j, drop=FALSE])
#'             V[, j + 1] <- F / norm2(F)
#'             R <- 0
#'           }
#'           else V[, j + 1] <- F / R
#'           
#'           #       Compute block diagonal matrix
#'           if (is.null(B)) B <- cbind(S, R)
#'           else            B <- rbind(cbind(B, 0), c(rep(0, ncol(B) - 1), S, R))
#'           
#'           jp1_w <- ifelse(w_dim > 1, j + 1, 1)
#'           w_old <- W[, j_w]
#'           
#'           #       Optionally apply scale
#'           VJP1 <- V[, j + 1]
#'           if (!missing(scale))
#'           {
#'             VJP1 <- VJP1 / scale
#'           }
#'           W[, jp1_w] <- drop(mult(A, drop(VJP1)))
#'           mprod <- mprod + 1
#'           
#'           #       Optionally apply shift
#'           if (!missing(shift))
#'           {
#'             W[, jp1_w] <- W[, jp1_w] + shift * VJP1
#'           }
#'           
#'           #       Optionally apply deflation
#'           if (deflate)
#'           {
#'             W[, jp1_w] <- W[, jp1_w] - ds * cross(dv, VJP1) * du
#'           }
#'           
#'           #       One step of the classical Gram-Schmidt process
#'           W[, jp1_w] <- W[, jp1_w] - R * w_old
#'           
#'           #       Full reorthogonalization of W
#'           if (reorth && w_dim > 1) W[, j + 1] <- orthog(W[, j + 1], W[, 1:j])
#'           S <- norm2(W[, jp1_w])
#'           #       Check for linear dependence
#'           if (S <= SVTol)
#'           {
#'             W[, jp1_w] <- rnorm(nrow(W))
#'             if (w_dim > 1) W[, j + 1] <- orthog(W[, j + 1], W[, 1:j])
#'             W[, jp1_w] <- W[, jp1_w] / norm2(W[, jp1_w])
#'             S <- 0
#'           }
#'           else W[, jp1_w] <- W[, jp1_w] / S
#'         }
#'         else
#'         {
#'           #       Add a last block to matrix B
#'           B <- rbind(B, c(rep(0, j - 1), S))
#'         }
#'         j <- j + 1
#'       }
#'       if (verbose)
#'       {
#'         message("\nLanczos iter = ", iter, " j = ", j - 1, "mprod = ", mprod)
#'       }
#'       # ---------------------------------------------------------------------
#'       # (End of the Lanczos bidiagonalization part)
#'       # ---------------------------------------------------------------------
#'       Bsz <- nrow(B)
#'       R_F <- norm2(F)
#'       F <- F / R_F
#'       #   Compute singular triplets of B. Expect svd to return s.v.s in order
#'       #   from largest to smallest.
#'       Bsvd <- svd(B)
#'       
#'       #   Estimate ||A|| using the largest singular value over all iterations
#'       #   and estimate the cond(A) using approximations to the largest and
#'       #   smallest singular values. If a small singular value is less than sqrteps
#'       #   require two-sided reorthogonalization.
#'       if (iter == 1)
#'       {
#'         Smax <- Bsvd$d[1]
#'         Smin <- Bsvd$d[Bsz]
#'       }
#'       else
#'       {
#'         Smax <- max(Smax, Bsvd$d[1])
#'         Smin <- min(Smin, Bsvd$d[Bsz])
#'       }
#'       Smax <- max(eps23, Smax)
#'       if ( (Smin / Smax < sqrteps) && !reorth)
#'       {
#'         warning("The matrix is ill-conditioned. Basis will be reorthogonalized.")
#'         reorth <- TRUE
#'       }
#'       
#'       #   Compute the residuals
#'       R <- R_F * Bsvd$u[Bsz, , drop=FALSE]
#'       #   Check for convergence
#'       ct <- convtests(Bsz, tol, k_org, Bsvd$u,
#'                       Bsvd$d, Bsvd$v, abs(R), k, SVTol, Smax)
#'       k <- ct$k
#'       
#'       #   If all desired singular values converged, then exit main loop
#'       if (ct$converged) break
#'       if (iter >= maxit) break
#'       
#'       #   Compute the starting vectors and first block of B[1:k, 1:(k+1), drop=FALSE]
#'       #   using the Ritz vectors
#'       V[, 1:(k + dim(F)[2])] <- cbind(V[, 1:(dim(Bsvd$v)[1]),
#'                                         drop=FALSE] %*% Bsvd$v[, 1:k], F)
#'       B <- cbind( diag(Bsvd$d[1:k], nrow=k), R[1:k])
#'       
#'       #   Update the left approximate singular vectors
#'       if (w_dim > 1)
#'       {
#'         W[, 1:k] <- W[, 1:(dim(Bsvd$u)[1]), drop=FALSE] %*% Bsvd$u[, 1:k]
#'       }
#'       
#'       iter <- iter + 1
#'     }
#'     # ---------------------------------------------------------------------
#'     # End of the main iteration loop
#'     # Output results
#'     # ---------------------------------------------------------------------
#'     if (iter > maxit) warning("did not converge, try increasing maxit")
#'     d <- Bsvd$d[1:k_org]
#'     if (!right_only)
#'     {
#'       u <- W[, 1:(dim(Bsvd$u)[1]), drop=FALSE] %*% Bsvd$u[, 1:k_org, drop=FALSE]
#'     }
#'     v <- V[, 1:(dim(Bsvd$v)[1]), drop=FALSE] %*% Bsvd$v[, 1:k_org, drop=FALSE]
#'     if (right_only)
#'       return(list(d=d, v=v[, 1:nv, drop=FALSE], iter=iter, mprod=mprod))
#'     return(list(d=d, u=u[, 1:nu, drop=FALSE],
#'                 v=v[, 1:nv, drop=FALSE], iter=iter, mprod=mprod))
#'   }
#'   
#' 
#' partial_eigen.big <- function(x, n=5, symmetric=TRUE, ...)
#' {
#'   if (n > 0.5 * min(nrow(x),ncol(x)))
#'   {
#'     warning("You're computing a large percentage of total eigenvalues, the standard eigen function will likely work better!")
#'   }
#'   if (!symmetric)
#'   {
#'     L <- irlba.big(x, n, ...)
#'     return(list(vectors=L$v, values=L$d ^ 2))
#'   }
#'   L <- irlba.big(x, n, ...)
#'   s <- sign(L$u[1,] * L$v[1,])
#'   if (all(s > 0))
#'   {
#'     return(list(vectors=L$u, values=L$d))
#'   }
#'   i <- min(which(s < 0))
#'   shift <- L$d[i]
#'   L <- irlba.big(x, n, shift=shift, ...)
#'   return(list(vectors=L$u, values=L$d - shift))
#' }
#' 
#' 
#' cross <- function(x,y)
#' {
#'   if(missing(y))
#'   {
#'     if(is.complex(x)) return(abs(Conj(t(x)) %*% x))
#'     return(crossprod(x))
#'   }
#'   if(!is.complex(x) && !is.complex(y)) return(crossprod(x,y))
#'   Conj(t(x)) %*% y
#' }
#' 
#' # Euclidean norm
#' norm2 <- function (x)
#' {
#'   drop(sqrt(cross(x)))
#' }
#' 
#' # Orthogonalize vectors Y against vectors X.
#' orthog <- function (Y, X)
#' {
#'   dx2 <- dim(X)[2]
#'   if(is.null(dx2)) dx2 <- 1
#'   dy2 <- dim(Y)[2]
#'   if(is.null(dy2)) dy2 <- 1
#'   if (dx2 < dy2) doty <- cross(X, Y)
#'   else doty <- Conj(t(cross(Y, X)))
#'   return (Y - X %*% doty)
#' }
#' 
#' # Convergence tests
#' # Input parameters
#' # Bsz            Number of rows of the bidiagonal matrix B
#' # tol
#' # k_org
#' # U_B            Left singular vectors of small matrix B
#' # S_B            Singular values of B
#' # V_B            Right singular vectors of B
#' # residuals
#' # k
#' # SVTol
#' # Smax
#' #
#' # Output parameter list
#' # converged      TRUE/FALSE
#' # U_B            Left singular vectors of small matrix B
#' # S_B            Singular values of B
#' # V_B            Right singular vectors of B
#' # k              Number of singular vectors returned
#' convtests <- function (Bsz, tol, k_org, U_B, S_B, V_B,
#'                        residuals, k, SVTol, Smax)
#' {
#'   len_res <- sum(residuals[1:k_org] < tol * Smax)
#'   if(is.na(len_res)) len_res <- 0
#'   if (len_res == k_org) {
#'     return (list(converged=TRUE, U_B=U_B[,1:k_org, drop=FALSE],
#'                  S_B=S_B[1:k_org, drop=FALSE],
#'                  V_B=V_B[,1:k_org, drop=FALSE], k=k))
#'   }
#'   # Not converged yet...
#'   # Adjust k to include more vectors as the number of vectors converge.
#'   len_res <- sum(residuals[1:k_org] < SVTol * Smax)
#'   k <- max(k, k_org + len_res)
#'   if (k > Bsz - 3) k <- max(Bsz - 3,1)
#'   return (list(converged=FALSE, U_B=U_B, S_B=S_B, V_B=V_B, k=k) )
#' }


#Function to collapse down labels
reduceLabels<-function(data,labelsToReduce,overallLabel){
  #labelsToReduce is a list, each element of which is a vector of strings giving the label names to be combined
  #overallLabel is a vector of strings, each entry gives the new overall name for the labels in the corresponding
  #list entry of labelsToReduce
  for (i in 1:length(overallLabel)){
    ix<-which(data$behavior %in% labelsToReduce[[i]])
    data$behavior[ix]<-overallLabel[i]
  }
  return(data)
}



RF_analysis<-function(ntrees,ncores,train_set,test_set,test_behavior,RFoutput,ResultsOutput,output_token=leave_out,replace=TRUE,sampsize = if (replace) nrow(train_set) else ceiling(.632*nrow(train_set)),save_vals=TRUE,run_preds=TRUE,create_RF=TRUE,made_rf=NULL){
  
  #create output dir is doesnt exist
  if(save_vals==TRUE){
    dir.create(ResultsOutput, showWarnings = FALSE)
    dir.create(RFoutput, showWarnings = FALSE)
  }
  
  if(create_RF==TRUE){
    newRF<- foreach(ntree=splitNumber(ntrees,nprocs = ncores ), .combine=randomForest::combine, .multicombine=TRUE, .packages='randomForest') %dopar%
      randomForest(x = train_set[,2:(ncol(train_set)-2),with=FALSE],y=as.factor(train_set$behavior),
                   ntree=ntree,
                   importance=FALSE,
                   proximity = FALSE,
                   do.trace = 100,
                   classwt=classwt,
                   sampsize=sampsize,
                   replace=replace)
    
    
    
    Testing_predictions<-predict(newRF,test_set[,2:(ncol(train_set)-2),with=FALSE])
  } else if(create_RF==FALSE){
    Testing_predictions<-Given_predictions
    newRF=made_rf
  }
  
  if(run_preds==TRUE){
    
    RF_conf<-confusionMatrix(Testing_predictions, test_behavior)
    
    
    print("RF accuracy")
    confusionMatrix(Testing_predictions, test_behavior)$overall
    #extract states
    
  }
    #Save Outputs
    if(save_vals==TRUE & create_RF==TRUE){
      save(RF_conf,file=file.path(ResultsOutput,paste0('RF_conf',output_token,'.RData')))
      save(newRF,file=file.path(RFoutput,paste0('RF_',output_token,'.RData')))
      
    }
    return(list(newRF))
    
}


number2binary = function(number, noBits) {
  binary_vector = rev(as.numeric(intToBits(number)))
  if(missing(noBits)) {
    return(binary_vector)
  } else {
    binary_vector[-(1:(length(binary_vector) - noBits))]
  }
}


HMM_analysis<-function(states,EmissionProbs,Testing_predictions,test_behavior,output_token,save_vals=TRUE,run_preds=TRUE,create_hmm=TRUE,HMM){
  
  if(create_HMM==TRUE){
  states<-states[!is.na(states)]
  
  #Calculate Empirial transition matrix
  statesLength<-length(states)
  Trans<-table(states[1:statesLength-1],states[2:statesLength])
  Trans <- Trans / rowSums(Trans)
  
  #Calculate priors
  initial_state_probabilities<-count(states)
  initial_state_probabilities<-initial_state_probabilities$freq/statesLength
  
  #Calculate emission probabilities

  unique_states = sort(unique(states))

  #intialise HMM
  hmm = initHMM(unique_states, unique_states, initial_state_probabilities, Trans, EmissionProbs)
  } else {
    hmm=HMM
  }
  
  if(save_vals==TRUE){
    save(hmm,file=file.path(HMMoutput,paste0('HMM_',output_token,'.RData')))
  }
  #hmmModel = trainHMM(train$label, RF)

  if(run_preds==TRUE){
  #Now run viterbi over state sequence
  filtered = viterbi(hmm, Testing_predictions)
  
  filtered_asfact<-as.factor(filtered)
  
  levels(filtered_asfact)<-c(levels(filtered_asfact),setdiff(levels(test_behavior),levels(filtered_asfact)))
  #test_behavior<-test_behavior[iq]
  
  HMM_conf<-confusionMatrix(filtered_asfact, test_behavior)
  
  save(HMM_conf,file=file.path(ResultsOutput,paste0('HMM_conf',output_token,'.RData')))
}

return(hmm)

}