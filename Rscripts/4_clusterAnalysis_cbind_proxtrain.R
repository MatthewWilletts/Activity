#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will combine the chunks of the training_node matrix

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out


#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-4

#set.seed(chunkID)

registerDoMC(ncores)

cat(ntrees)
cat(nchunks)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,'participants.RData'))


ProxTrain<-rbind_prox_files(inputDirectory=NodeOutput,outputDirectory=NodeOutput,startToken='ProxTrain_',leftOutParticipant=participants[leave_out],nchunks=nchunks)

cat(paste0('number of rows is ',nrow(ProxTrain)))
cat(paste0('number of cols is ',ncol(ProxTrain)))

if(!file.exists(file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))){
  
  write.csv(x = ProxTrain,file =file.path(NodeOutput,paste0('ProxTrain_',participants[leave_out],'.csv')),row.names = FALSE )
  
}
