#MultiCore analysis on ARCUS B cluster of 120 or so participants labelled data, using the Holmes Algorithm

#load packages
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')


#In this script we will be doing 'leave one out' analysis for our participants


source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterFunctions.R')


#Take in arguements from command line
#This script will calculate one chunk of the training_node matrix for ntree/nchunks rf trees

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

#load data
AllData<-fread(input = file.path(outputDataDirectory,paste0('AllData_',duration,'.csv')))

#turn into matrix of features and a vector of behaviors

AllBehaviorData<-as.vector(AllData[,behavior])
AllIdentifierData<-as.vector(AllData[,identifier])

AllData<-as.matrix(AllData[,4:ncol(AllData),with=FALSE])


#write participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))


iq<-which(AllIdentifierData==participants[leave_out])

#now only analyse ntree/chunk of data - using splitnumber
ntree_for_chunk<-splitNumber(ntrees,nchunks)[chunkID]


RFoutput<-RF_nodes_chunk(TrainingFData=AllData[-iq,],TrainingBData=AllBehaviorData[-iq],TestingFData=AllData[iq,],ncores=ncores,ntree=ntree_for_chunk,savefileloc=RFoutput,chunkID=chunkID,nametoken =participants[leave_out] )




