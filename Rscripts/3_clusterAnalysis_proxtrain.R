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

#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will calculate a chunk of the ProxTrain matrix

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out


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
RFoutput<-paste0(resultsDataDirectory,'/RFoutput')

#load participants
load(file =file.path(resultsDataDirectory,'participants.RData'))

#load data - Training Nodes
training_nodes_all<-fread(input=file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))

training_nodes_matrix<-as.matrix(training_nodes_all)

rm(training_nodes_all)

#now only analyse a chunk of data - using chunkOfMatrix
training_nodes_part<-chunkOfMatrix(data_matrix = training_nodes_matrix,nchunks =nchunks,chunkID =  chunkID)

training_nodes_part_chunked<-splitMatrix(training_nodes_part,nprocs = ncores)

rm(training_nodes_part)
#Output the RF proximity matrix, for all testing data training data - ie for one participant

#Proximity matrix is npoints by npoints
cat(paste0('calculating training nodes proximity \n'))

ProxTrain <- foreach(splitTraining=training_nodes_part_chunked, .combine = rbind) %dopar% matrix(
  computeProximityC(nodes1=training_nodes_all,nodes2=splitTraining),
  nrow=nrow(splitTraining),dimnames=list(rownames(splitTraining)))

ProxTrain<-ProxTrain[order(as.numeric(rownames(ProxTrain))),]


if(!file.exists(file.path(RFoutput,paste0('ProxTrain_',participants[leave_out],'.csv')))){
  write.csv(x = ProxTrain,file =file.path(RFoutput,paste0('ProxTrain_',participants[leave_out],'.csv')),row.names = FALSE )
}


