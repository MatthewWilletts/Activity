#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will take in the training_node matrix
#Then pick one in 6 data points from within each behahviour
#then save the resulting training_node matrix
#and the indices of the picked points

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out


#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-16

#set.seed(chunkID)

registerDoMC(ncores)

cat(ntrees)
cat(nchunks)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')

#load participants
load(file =file.path(resultsDataDirectory,'participants.RData'))


training_nodes_all<-fread(input = file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))

training_nodes_matrix<-as.matrix(training_nodes_all)

rm(training_nodes_all)

cat(paste0('number of rows is ',nrow(training_nodes)))
cat(paste0('number of cols is ',ncol(training_nodes)))

AllData<-fread(input = file.path(outputDataDirectory,'AllData.csv'))

#turn into matrix of features and a vector of behaviors
AllBehaviorData<-as.vector(AllData[,behavior])
AllIdentifierData<-as.vector(AllData[,identifier])

AllData<-as.matrix(AllData[,4:ncol(AllData),with=FALSE])

iq<-which(AllIdentifierData==participants[leave_out])

set.seed(leave_out)

behaviors<-unique(AllBehaviorData)

IndexByBehaviour<-lapply(X = 1:length(behaviors),FUN = function(x) which(AllBehaviorData[-iq]==behaviors[x]))

#keep every 6th
SubSampledIndexes<-lapply(X = IndexByBehaviour,FUN = function(x) sample(x = x,size = floor(length(x)/6),replace = FALSE))

SubSampled_training_nodes<-lapply(SubSampledIndexes, FUN = function(x) training_nodes_matrix[x,])

SubSampled_training_nodes_matrix<-do.call(rbind,SubSampled_training_nodes)

if(!file.exists(file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'subsampled.csv')))){
  
  write.csv(x = SubSampled_training_nodes_matrix,file =file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'_subsampled.csv')),row.names = FALSE )
  
}
