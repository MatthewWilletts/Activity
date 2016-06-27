source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterPackages.R')



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

registerDoMC(ncores)

#define data directories
source('/home/dph-ukbaccworkgroup/magd4534/Activity/clusterDirectories.R')


#load participants
load(file =file.path(resultsDataDirectory,paste0('participants_',duration,'.RData')))

#now we need to attach the CV matrix
CVDescriptorFile<-paste0('CVBackingFile_',participants[leave_out],'.desc')


CV<-attach.big.matrix(file.path(ProxOutput,CVDescriptorFile))

#Now compute first 10 eigenvectors

#Need to define matrix multiplication for big matrices:
matmul = function(A, B, transpose=FALSE)
{
  # Bigalgebra requires matrix/vector arguments
  if(is.null(dim(B))) B = cbind(B)
  
  if(transpose)
    return(cbind((t(B) %*% A)[]))
  
  cbind((A %*% B)[])
}



cat('calculating eigenvectors \n')
Eigenvectors<-partial_eigen(CV, n = 10, symmetric = TRUE,mult=matmul)

cat('saving \n')
save(Eigenvectors,file = file.path(ProxOutput,'eigenvectors.RData'))