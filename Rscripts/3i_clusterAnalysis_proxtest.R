#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will calculate a chunk of the ProxTest matrix

#So we need to know:
#a) how many chunks job is divided into
#b) which chunk we are doing
#c) which participant we are leaving out


#parse inputs
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterInputs.R')


ncores<-2

set.seed(chunkID)

registerDoMC(ncores)

#First, define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')

#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

#load data - Training Nodes
training_nodes_all<-fread(input=file.path(RFoutput,paste0('training_nodes_',participants[leave_out],'.csv')))

training_nodes_matrix<-as.matrix(training_nodes_all)


#load data - Testing Nodes
testing_nodes_all<-fread(input=file.path(RFoutput,paste0('testing_nodes_',participants[leave_out],'.csv')))

testing_nodes_matrix<-as.matrix(testing_nodes_all)

rm(testing_nodes_all)

#now only analyse a chunk of data - using chunkOfMatrix
testing_nodes_part<-chunkOfMatrix(data_matrix = testing_nodes_matrix,nchunks =nchunks,chunkID =  chunkID)

testing_nodes_part_chunked<-splitMatrix(testing_nodes_part,nprocs = ncores)

rm(testing_nodes_part)
#Output the RF proximity matrix, for all testing data training data - ie for one participant

#Proximity matrix is npoints by npoints
cat(paste0('calculating testing nodes proximity \n'))

start.time <- Sys.time()


ProxTest <- foreach(splitTesting=testing_nodes_part_chunked,.combine = rbind) %dopar% matrix(
  computeProximityC_int(nodes1=training_nodes_matrix,nodes2=splitTesting),
  nrow=nrow(splitTesting),dimnames=list(rownames(splitTesting)))


# ProxTest <- foreach(splitTraining=training_nodes_part_chunked, .combine = rbind) %dopar% 
#   computeProximityC_int(nodes1=training_nodes_matrix,nodes2=splitTraining)

ProxTest<-ProxTest[order(as.numeric(rownames(ProxTest))),]

end.time <- Sys.time()

time.taken <- end.time - start.time

nrow_proxtest<-nrow(ProxTest)

save(time.taken,file = file.path(RFoutput,paste0('ProxTest_time_',chunkID,'_',participants[leave_out],'.RData')))
save(nrow_proxtest,file = file.path(RFoutput,paste0('ProxTest_nrow_',chunkID,'_',participants[leave_out],'.RData')))

if(!file.exists(file.path(RFoutput,paste0('ProxTest_',participants[leave_out],'.csv')))){
  write.csv(x = ProxTest,file =file.path(RFoutput,paste0('ProxTest_',chunkID,'_',participants[leave_out],'.csv')),row.names = FALSE )
}


