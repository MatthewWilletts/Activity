#Function for cleaning up raw bout data to machine-readible format for analysis
#cleanData <- function(jointFiles=jointFiles,labelDirectory=labelDirectory,dataDirectory=dataDirectory,drop=dropvals,outputLabelDir=outputLabelDir,instanceLabelDirectory=instanceLabelDirectory){

cleanData <- function(jointFiles,labelDirectory,dataDirectory,drop=dropvals,outputLabelDir,instanceLabelDirectory){
  
  
  participantID<-gsub(jointFiles[4],pattern = ".csv",replacement = '')
  
  if(file.exists(file.path(instanceLabelDirectory,jointFiles[4]))==FALSE){
    
    labelData<-fread(file.path(labelDirectory,jointFiles[1]),drop=dropvals)
    
    labelData$NewStart<-(as.POSIXct(labelData$startTime,'%Y-%m-%d %H:%M:%S',tz = 'GMT'))
    labelData$tempEnd<-as.POSIXct(labelData$endTime,'%Y-%m-%d %H:%M:%S',tz = 'GMT')
    labelData$duration<-labelData$tempEnd-labelData$NewStart
    if(sum(labelData$duration==0)>0){
      labelData<-labelData[-(labelData$duration==0),]
    }
    labelData$NewEnd<-labelData$tempEnd
    
    nrows<-nrow(labelData)
    #Now relabel start and end points to lie in the midpoint of unlabelled gaps
    
    labelData$NewEnd[1:nrows-1]<-labelData$NewEnd[1:nrows-1]+floor(labelData$NewStart[2:nrows]-labelData$NewEnd[1:nrows-1])/2
    labelData$NewEnd[nrows]<-labelData$tempEnd[nrows]
    
    labelData$NewStart[2:nrows]<-labelData$NewEnd[1:nrows-1]
    
    labelData<-labelData[,c(1,4,5,8),with=FALSE]
    colnames(labelData)<-c('identifier','behavior','StartDateTime','EndDateTime')
    
    #Load up Feature Data
    featureData<-fread(file.path(dataDirectory,jointFiles[2]))
    
    #Load up FFT Data
    FFTData<-fread(file.path(dataDirectory,jointFiles[3]))
    
    
    
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
    
    #store data points outside labelled epoch
    labelledFeatureData<-featureData[start_row:end_row,]
    labelledFFTData<-FFTData[start_row:end_row,]
    
    #now round labels to align with spacing of features
    labelData$StartDateTime<-round5secs(labelData$StartDateTime,startTime = labelData$StartDateTime[1])
    labelData$EndDateTime<-round5secs(labelData$EndDateTime,startTime = labelData$StartDateTime[1])
    
    #now we need to turn from bout level labels to annotations. Here we use a slightly modified TLBC function, annotationsToLabels
    extractLabelsSingleFile(labelData,winSize = 5,instanceLabelDirectory = instanceLabelDirectory)
    
    instanceLabelData<-fread(file.path(instanceLabelDirectory,jointFiles[4]))
    
    
    #discard all feaure data before first labelled data point and after last labelled point
    kx<-which(!instanceLabelData$behavior=='unknown')
    if(length(kx)>0){
      maxIndex<-min(kx[length(kx)],nrow(labelledFeatureData))
      instanceLabelData<-instanceLabelData[kx[1]:maxIndex,]
      
      labelledFeatureData<-labelledFeatureData[kx[1]:maxIndex,]
      labelledFFTData<-labelledFFTData[kx[1]:maxIndex,]
    }
    
    print(paste0('Writing instance data file ',instanceLabelData$identifier[1],'.csv'))
    write.csv(x=instanceLabelData,file=paste0(instanceLabelDirectory,'/',labelData$identifier[1],'.csv'),row.names=FALSE,append = FALSE)
    
    print(paste0('Writing feature data file ',labelledFeatureData$identifier[1],'.csv'))
    write.csv(x=labelledFeatureData,file=paste0(dataDirectory,'/',labelData$identifier[1],'CleanFeature.csv'),row.names=FALSE,append = FALSE)
    
    print(paste0('Writing FFT data file ',labelledFFTData$identifier[1],'.csv'))
    write.csv(x=labelledFFTData,file=paste0(dataDirectory,'/',labelData$identifier[1],'CleanFFT.csv'),row.names=FALSE,append = FALSE)
    
  } else {
    print(paste0('error: files for ',labelData$identifier[1],'.csv already exists'))
  }
  
  return(list(instanceLabelData,labelledFeatureData,labelledFFTData))
}

#Function for rounding to the nearest 5 seconds, based at the first time entry (ie first time is 00:00:02, all times will be rounded to end with 2 or 7 seconds)
round5secs <- function( x,startTime) { 
  start<-as.POSIXlt(startTime)
  startSecs<-start$sec
  x <- as.POSIXlt( x - startSecs+ as.difftime( 2.5, units="secs" ) )
  x$sec <- 5*(x$sec %/% 5) 
  as.POSIXct(x+startSecs)
} 


extractLabelsSingleFile = function(all_bouts, winSize,instanceLabelDirectory) {
  
  dateFmt = '%Y-%m-%d %H:%M:%S'
  tz = 'GMT'
  
  annotations = unique(all_bouts$behavior)
  actNames = sub(" ", "", annotations)
  identifiers = unique(all_bouts$identifier)
  for (id in 1:length(identifiers)) {
    cat(identifiers[id], "\n")
    
    bouts = all_bouts[all_bouts$identifier == identifiers[id], ]
    outputFile = file.path(instanceLabelDirectory, paste0(identifiers[id],'.csv'))
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
    cat("timestamp,behavior\n", file=out, append=TRUE)
    
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize <= boutstop)) {
        # the window is within this bout - add the label
        label = sub(" ", "", str_trim(bouts[r, c('behavior'),with=FALSE]))
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
          label = sub(" ", "", str_trim(bouts[r, c('behavior'),with=FALSE]))
        }
      }
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
