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
#This script will calculate one chunk of the training_node matrix for ntree/nchunks rf trees

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
              metavar="number"),
  make_option(c("-t", "--trees"), type="integer", default=1000,
              help="number of trees for random forest",
              metavar="number")
)

opt <- parse_args(OptionParser(option_list=option_list))

nchunks<-opt$nchunks

chunkID<-opt$chunk

leave_out<-opt$participant

ntrees<-opt$trees

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


#load data
AllData<-fread(input = file.path(outputDataDirectory,'AllData.csv'))

#turn into matrix of features and a vector of behaviors
AllBehaviorData<-c(AllData[,3,with=FALSE])

AllData<-as.matrix(AllData[,4:ncol(AllData),with=FALSE])


#write participants
load(file =file.path(resultsDataDirectory,'participants.RData'))


iq<-which(AllData$identifier==participants[leave_out])

#now only analyse ntree/chunk of data - using splitnumber
ntree_for_chunk<-splitNumber(ntrees,nchunks)[chunkID]


RFoutput<-RF_nodes_chunk(TrainingFData=AllData[-iq,],TrainingBData=AllBehaviorData[-iq],TestingFData=AllData[iq,],ncores=ncores,ntree=ntree_for_chunk,savefileloc=RFoutput,chunkID=chunkID,nametoken =participants[leave_out] )
