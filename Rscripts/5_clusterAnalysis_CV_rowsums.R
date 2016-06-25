#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm


#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')



#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will read in the ProxTrain matrix as a bigmemory matrix and do eigenvector analysis on it

#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-16

set.seed(chunkID)

registerDoMC(ncores)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

ProxTrainDescriptorFile<-file.path(ProxOutput,paste0('ProxTrainBackingFile_',participants[leave_out],'.desc'))

RowSum<-foreach(corenumber=1:2, .combine = c) %dopar% ComputeRowSumMatrixChunk(
  Proximity=ProxTrainDescriptorFile,nchunks=nchunks,chunkID=chunkID,ncores=ncores,coreID=corenumber)


#save results

write.csv(x =  RowSum,file  = file.path(ProxOutput,paste0('CV_rowsums_',participants[leave_out],'_',chunkID,'_subsampled.csv')),row.names = FALSE)

