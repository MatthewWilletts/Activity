#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')



#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will calculate a chunk of the ProxTrain matrix

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out

#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-16

set.seed(chunkID)

registerDoMC(ncores)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

#load mean values

rowmeanvalues<-bind_sum_files(inputDirectory =ProxOutput,startToken ='CV_rowsums',leftOutParticipant =participants[leave_out],nchunks = nchunks)
colmeanvalues<-bind_sum_files(inputDirectory =ProxOutput,startToken ='CV_colsum',leftOutParticipant = participants[leave_out],nchunks = nchunks)

meanvalue<-sum(rowmeanvalues)
rowmeanvalues<-rowmeanvalues/length(colmeanvalues)
colmeanvalues<-colmeanvalues/length(rowmeanvalues)
meanvalue<-meanvalue/(length(colmeanvalues)*length(rowmeanvalues))


ProxTrainDescriptorFile<-paste0('ProxTrainBackingFile_',participants[leave_out],'.desc')

CVDescriptorFile<-paste0('CVBackingFile_',participants[leave_out],'.desc')


#Now calculate CV matrix piecewise

listofEndRows<- foreach(corenumber=1:ncores, .combine = rbind) %dopar% computeCVhalfbigmatrix()
  CV.bigmatrix.descfilepath =file.path(ProxOutput,CVDescriptorFile)
  ,rowmeanvalues=rowmeanvalues,colmeanvalues = colmeanvalues,meanvalue = meanvalue
  ,nchunks = nchunks,chunkID = chunkID,ncores = ncores,coreID = corenumber)
