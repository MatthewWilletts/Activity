#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
library(data.table)
library(Matrix)


#In this script we will be doing 'leave one out' analysis for our participants


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

registerDoMC(ncores/2)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

#load mean values

ProxTrain_dt<-fread(input =file.path(ProxOutput,paste0('ProxTrain_',participants[leave_out],'.csv')),skip=1,header = FALSE)
ProxTrain_matrix<-as.matrix(ProxTrain_dt)
rm(ProxTrain_dt)

#Convert to sparse
ProxTrain_sparse<-Matrix(ProxTrain_matrix,sparse=TRUE)

rm(ProxTrain_matrix)

cat('conversion complete \n')

cat('saving \n')

writeMM(ProxTrain_sparse,file.path(ProxOutput,'ProxTrain_sparse'))

