#Various functions used in analysis


#Function for cleaning up raw bout data to machine-readible format for analysis
cleanLabelData <- function(labelDirectory=labelDirectory,labelFile,cleanLabelDirectory=cleanLabelDirectory){
 
labelData<-read.csv(paste0(labelDirectory,labelFile))
labelData$NewStart<-strptime(labelData$startTime,'%m/%d/%Y %H:%M',tz = 'GMT')
labelData$tempEnd<-strptime(labelData$endTime,'%m/%d/%Y %H:%M',tz = 'GMT')
labelData$duration<-labelData$tempEnd-labelData$NewStart
labelData<-labelData[-(labelData$duration==0),]
labelData$NewEnd<-labelData$tempEnd[length(labelData$NewStart)]
labelData$NewEnd[1:(length(labelData$NewStart)-1)]<-labelData$NewStart[2:(length(labelData$NewStart))]
labelData$NewEnd[length(labelData$NewStart)]<-labelData$tempEnd[length(labelData$NewStart)]
labelData<-labelData[,keeps]
colnames(labelData)<-c('identifier','StartDateTime','EndDateTime','behavior')
if(file.exists(paste0(cleanLabelDirectory,labelData$identifier[1],'.csv'))==FALSE){
	print(paste0('Writing file ',labelData$identifier[1],'.csv'))
	write.csv(x=labelData,file=paste0(cleanLabelDirectory,labelData$identifier[1],'.csv'),row.names=FALSE)
	} else {
	print(paste0('error: file ',labelData$identifier[1],'.csv already exists'))
	}

FirstLast=data.frame(labelData$identifier[1],labelData$StartDateTime[1],labelData$EndDateTime[length(labelData$identifier)])

return(FirstLast)
}



#Function for cleaning up raw accelerometer data to machine-readible format for analysis
cleanAccelData<-function(FirstLast=FirstLast,accelFiles=accelFiles,dataDirectory=dataDirectory,cleanDataDirectory=cleanDataDirectory){


  if(dir.exists(cleanDataDirectory)==FALSE){
    dir.create(cleanDataDirectory)
  }
  for (i in 1:length(accelFiles)){

      #only proceed if output file doesn't exist
      if(file.exists(file=paste0(cleanDataDirectory,FirstLast$identifier[i],'.csv'))==FALSE){
      
      
      print(paste0('reading in file ',accelFiles[i]))

      data<-fread(input=paste0(dataDirectory,accelFiles[i]))

      start_row<-pmatch(paste0(as.character.POSIXt(FirstLast$First[i]),'.00'),data$V1,duplicates.ok = TRUE)

      end_row<-pmatch(paste0(as.character.POSIXt(FirstLast$Last[i]),'.00'),data$V1,duplicates.ok = TRUE)


      #delete data points outside labelled epoch
      data<-data[start_row:end_row,]


      #delete unneccesary columns
      data[,V6:=NULL]
      data[,V5:=NULL]
      data[,V1:=NULL]

      data=round(data,3)

      #cut down from 100Hz data to new rate (new rate must be a factor of 100)

      ind<-seq(1,nrow(data),by=100/rate)
      data<-data[ind,]

      duration=as.numeric(FirstLast$Duration[i])

      #Now create header needed for TLBC
lines=paste0('------------ Data File Created By ActiGraph GT3X+ ActiLife v6.3.0 Firmware v2.0.0 date format M/d/yyyy at ',rate,' Hz  Filter Normal -----------
Serial Number: MRA1CXXXXXXXX
Start Time ',strftime(FirstLast$First[i],format='%H:%M:%S'),'
Start Date ',strftime(FirstLast$First[i],format='%m/%d/%Y'),'
Epoch Period (hh:mm:ss) ',paste0(str_pad(floor(duration), width=2, pad="0"),':',str_pad(round((duration-floor(duration))*60), width=2, pad="0"),':00'),'
Download Time 00:00:00
Download Date 1/1/2016
Current Memory Address: 0
Current Battery Voltage: 0     Mode = 0
--------------------------------------------------')

  
      print(paste0('Writing file ',FirstLast$identifier[i],'.csv'))
      
      write(x=lines,file=paste0(cleanDataDirectory,FirstLast$identifier[i],'.csv'))
      
      write.table(x=data,file=paste0(cleanDataDirectory,FirstLast$identifier[i],'.csv'),append = TRUE,row.names = FALSE,col.names = FALSE,sep=',') 
    
      } else { print(paste0('error: file ',FirstLast$identifier[i],'.csv already exists'))}

#end of loop
rm (data)
}

#end of function
return('complete')
}
