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
#This script will combine the chunks of the training_node matrix

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out


option_list <- list(
  make_option(c("-n", "--nchunks"), type="integer",
              help="number of chunks data is divided into",
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


leave_out<-opt$participant

ntrees<-opt$trees

ncores<-16

#set.seed(chunkID)

registerDoMC(ncores)

cat(ntrees)
cat(nchunks)
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


#Directories for RF
RFoutput<-paste0(resultsDataDirectory,'/RFoutput')

#load participants
load(file =file.path(resultsDataDirectory,'participants.RData'))


training_nodes<-cbind_node_files(inputDirectory=RFoutput,outputDirectory=RFoutput,startToken='training_nodes_',leftOutParticipant=participants[leave_out])

cat(paste0('number of rows is ',nrow(training_nodes)))
cat(paste0('number of cols is ',ncol(training_nodes)))

if(!file.exists(file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))){
  
write.csv(x = training_nodes,file =file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')),row.names = FALSE )

}

if(!file.exists(file.path(RFoutput,paste0('testing_nodes_',participants[leave_out],'.csv')))){
 

  testing_nodes<-cbind_node_files(inputDirectory=RFoutput,outputDirectory=RFoutput,startToken='testing_nodes_')
  
  write.csv(x = testing_nodes,file =file.path(RFoutput,paste0('testing_nodes_',participants[leave_out],'.csv')),row.names = FALSE )
  
}




