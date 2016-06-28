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


ncores<-2

set.seed(chunkID)

registerDoMC(ncores)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))


ProxTest_dt<-fread(input =file.path(ProxTestOutput,paste0('ProxTest_',participants[leave_out],'.csv')),skip=1,header = FALSE)
ProxTest_matrix<-as.matrix(ProxTest_dt)
rm(ProxTest_dt)

#calc CV

CVProxTest<-computeCVmatrix(ProxTest_matrix)

if(!file.exists(file.path(ProxTestOutput,paste0('CVProxTest_',participants[leave_out],'.csv')))){
  write.csv(x = CVProxTest,file =file.path(ProxTestOutput,paste0('CVProxTest_',chunkID,'_',participants[leave_out],'.csv')),row.names = FALSE )
}
