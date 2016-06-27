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

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

#load mean values

rowmeanvalues<-read.csv(file.path(ProxOutput,'CV_rowsums_p002.csv'))
rowmeanvalues<-rowmeanvalues$x
colmeanvalues<-rowmeanvalues

meanvalue<-sum(rowmeanvalues)
rowmeanvalues<-rowmeanvalues/length(colmeanvalues)
colmeanvalues<-colmeanvalues/length(rowmeanvalues)
meanvalue<-meanvalue/(length(colmeanvalues)*length(rowmeanvalues))

ProxTrain_dt<-fread(input =file.path(ProxOutput,paste0('ProxTrain_',participants[leave_out],'.csv')),skip=1,header = FALSE)
ProxTrain_matrix<-as.matrix(ProxTrain_dt)
rm(ProxTrain_dt)


ProxTrain_matrix_part<-chunkOfMatrix_cols(data_matrix = ProxTrain_matrix,nchunks =nchunks,chunkID =  chunkID)

ProxTrain_matrix_part_chunk<-splitMatrix_cols(data_matrix = ProxTrain_matrix_part,nprocs =ncores )

rm(ProxTrain_matrix_part)



CVDescriptorFile<-paste0('CVBackingFile_',participants[leave_out],'.desc')


#Now calculate CV matrix piecewise

listofEndRows<- foreach(corenumber=1:ncores, .combine = rbind) %dopar% computeCVhalfbigmatrix(
  Proximity.matrix=ProxTrain_matrix_part_chunk,CV.bigmatrix.descfilepath =file.path(ProxOutput,CVDescriptorFile),
  rowmeanvalues=rowmeanvalues,colmeanvalues = colmeanvalues,meanvalue = meanvalue,
  nchunks = nchunks,chunkID = chunkID,ncores = ncores,coreID = corenumber)
