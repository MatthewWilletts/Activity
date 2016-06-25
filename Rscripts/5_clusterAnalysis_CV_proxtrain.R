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

#load data - ProxTrain
#ProxTrain_matrix<-read.big.matrix(filename =file.path(ProxOutput,paste0('ProxTrain_',participants[leave_out],'.csv')),skip=1,header = FALSE,type = 'double',backingpath = ProxOutput,backingfile = paste0('ProxTrainBackingFile_',participants[leave_out]))
  ProxTrain_dt<-fread(input =file.path(ProxOutput,paste0('ProxTrain_',participants[leave_out],'.csv')),skip=1,header = FALSE)
  ProxTrain_matrix<-as.matrix(ProxTrain_dt)

#create output matrix - CV
CV_matrix<-filebacked.big.matrix(nrow=nrow(ProxTrain_matrix),ncol = ncol(ProxTrain_matrix),type = 'double',backingpath = ProxOutput,backingfile = paste0('CVBackingFile_',participants[leave_out]))

#calculate mean values by row, column and overall
rowmeanvalues<-rowsum(ProxTrain_matrix)

meanvalue<-sum(rowmeanvalues)/(ncol(ProxTrain_matrix)*nrow(ProxTrain_matrix))

rowmeanvalues<-rowmeanvalues/ncol(ProxTrain_matrix)

colmeanvalues<-colsum(x = ProxTrain_matrix)
colmeanvalues<-colmeanvalues/nrow(ProxTrain_matrix)

#save results

write.csv(x =  rowmeanvalues,file  = file.path(ProxOutput,paste0('CV_rowmeans_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)
write.csv(x =  colmeanvalues,file  = file.path(ProxOutput,paste0('CV_colmeans_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)
write.csv(x =  meanvalue,file  = file.path(ProxOutput,paste0('CV_totmean_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)

