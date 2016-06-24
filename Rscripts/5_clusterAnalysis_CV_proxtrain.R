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
library(irlba)
library(bigmemory)
library(biganalytics)
library(bigalgebra)


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will read in the ProxTrain matrix as a bigmemory matrix and do eigenvector analysis on it


option_list <- list(
  make_option(c("-n", "--nchunks"), type="integer",
              help="number of chunks data is divided into",
              metavar="number"),
  make_option(c("-c", "--chunk"), type="integer",
              help="which chunk of data we are analysing",
              metavar="number"),
  make_option(c("-p", "--participant"), type="integer", default=1,
              help="which participant we are leaving out of analysis",
              metavar="number")
)

opt <- parse_args(OptionParser(option_list=option_list))

nchunks<-opt$nchunks

chunkID<-opt$chunk

leave_out<-opt$participant

ncores<-16

set.seed(chunkID)

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
ProxOutput<-paste0(resultsDataDirectory,'/RFoutput/ProxTrain')

#load participants
load(file =file.path(resultsDataDirectory,'participants.RData'))

#load data - ProxTrain
ProxTrain_matrix<-read.big.matrix(filename =file.path(ProxOutput,paste0('ProxTrain_',participants[leave_out],'_subsampled.csv')),skip=1,header = FALSE,type = 'double',backingpath = ProxOutput,backingfile = paste0('ProxTrainBackingFile_',participants[leave_out]))

#create output matrix - CV
CV_matrix<-filebacked.big.matrix(nrow=nrow(ProxTrain_matrix),ncol = ncol(ProxTrain_matrix),type = 'double',backingpath = ProxOutput,backingfile = paste0('CVBackingFile_',participants[leave_out]))

#calculate mean values by row, column and overall
rowmeanvalues<-BigRowSums(pBigMat = ProxTrain_matrix@address)

meanvalue<-sum(rowmeanvalues)/(ncol(ProxTrain_matrix)*nrow(ProxTrain_matrix))

rowmeanvalues<-rowmeanvalues/ncol(ProxTrain_matrix)

colmeanvalues<-colsum(x = ProxTrain_matrix)
colmeanvalues<-colmeanvalues/nrow(ProxTrain_matrix)

#save results

write.csv(x =  rowmeanvalues,file  = file.path(ProxOutput,paste0('CV_rowmeans_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)
write.csv(x =  colmeanvalues,file  = file.path(ProxOutput,paste0('CV_colmeans_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)
write.csv(x =  meanvalue,file  = file.path(ProxOutput,paste0('CV_totmean_',participants[leave_out],'_subsampled.csv')),row.names = FALSE)

