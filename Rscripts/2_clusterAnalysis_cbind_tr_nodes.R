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


ncores<-16

#set.seed(chunkID)

registerDoMC(ncores)

cat(paste0(ntrees,'\n'))
cat(paste0(nchunks,'\n'))


#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')

#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))


training_nodes<-cbind_node_files(inputDirectory=RFoutput,outputDirectory=RFoutput,startToken='training_nodes_',leftOutParticipant=participants[leave_out],nchunks=nchunks)

cat(paste0('number of rows is ',nrow(training_nodes),'\n'))
cat(paste0('number of cols is ',ncol(training_nodes),'\n'))

if(!file.exists(file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))){
  
write.csv(x = training_nodes,file =file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')),row.names = FALSE )

}

if(!file.exists(file.path(RFoutput,paste0('testing_nodes_',participants[leave_out],'.csv')))){
 

  testing_nodes<-cbind_node_files(inputDirectory=RFoutput,outputDirectory=RFoutput,startToken='testing_nodes_',leftOutParticipant=participants[leave_out],nchunks=nchunks)
  
  write.csv(x = testing_nodes,file =file.path(RFoutput,paste0('testing_nodes_',participants[leave_out],'.csv')),row.names = FALSE )
  
}




