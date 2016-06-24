#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


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

registerDoMC(ncores)

#First, define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')

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

start.time <- Sys.time()

ProxTrain <- foreach(splitTraining=training_nodes_part_chunked, .combine = rbind) %dopar% matrix(
  computeProximityC_int(nodes1=training_nodes_matrix,nodes2=splitTraining),
  nrow=nrow(splitTraining),dimnames=list(rownames(splitTraining)))

# ProxTrain <- foreach(splitTraining=training_nodes_part_chunked, .combine = rbind) %dopar% 
#   computeProximityC_int(nodes1=training_nodes_matrix,nodes2=splitTraining)

ProxTrain<-ProxTrain[order(as.numeric(rownames(ProxTrain))),]

end.time <- Sys.time()

time.taken <- end.time - start.time

nrow_proxtrain<-nrow(ProxTrain)

save(time.taken,file = file.path(RFoutput,paste0('ProxTrain_time_',chunkID,'_',participants[leave_out],'.RData')))
save(nrow_proxtrain,file = file.path(RFoutput,paste0('ProxTrain_nrow_',chunkID,'_',participants[leave_out],'.RData')))

if(!file.exists(file.path(RFoutput,paste0('ProxTrain_',participants[leave_out],'.csv')))){
  write.csv(x = ProxTrain,file =file.path(RFoutput,paste0('ProxTrain_',chunkID,'_',participants[leave_out],'.csv')),row.names = FALSE )
}


