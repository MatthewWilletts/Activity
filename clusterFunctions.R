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



# 
# 
# P <- rbind(mclapply(
#   nodes1[,],
#   function(a){apply(
#     nodes2,
#     MARGIN = 1,
#     function(b) sum(a==b))
#   }
#   ,mc.cores = mc.cores
# ))
# return(matrix(unlist(P), ncol = length(P[[1]]), byrow = TRUE))
# 
# outerFunct <- function(x, cores, funceval){
#   require(parallel)
#   mclapply(1:cores, funceval)
# }
# 
# require(doMC)  
# 
# registerDoMC(mc.cores)
# 
# 
# P <- foreach(b=nodes2, .combine='cbind') %:%
#   foreach(a=nodes1, .combine='c') %dopar% {
#     sum(a==b)
#   }
# 
# 
# func1 <- 'NumericMatrix mmult1(NumericMatrix a, NumericMatrix b) {
#   int acoln = a.ncol();
#   int bcoln = b.ncol();
#   NumericMatrix out = no_init_matrix(a.nrow(), acoln + bcoln);
#   for (int j = 0; j < acoln + bcoln; j++) {
#     if (j < acoln) {
#       out(_, j) = a(_, j);
#     } else {
#       out(_, j) = b(_, j - acoln);
#     }
#   }
#   return out;
# }'
# 
# cppFunction(func1)
# 
