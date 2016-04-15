library(data.table)
library(wavelets)
library(HMM)


#data<-fread(input='/Users/Matthew/Documents/Oxford/Holmes/Prototype_data/test1.csv')
data<-read.csv(file='/Users/Matthew/Documents/Oxford/Holmes/Prototype_data/p064ActivityEpoch_raw.csv',header=FALSE)
data$V6<-NULL

data<-data[c( rep(FALSE,4), TRUE ),]

small_data_wvlt<-dwt(data$V5[1:2^15])
data_wvlt<-dwt(data$V5)
test<-Matrix(0,742,7)
for(i in 1:7)
{
  test[,i]=rep(unlist(data_wvlt@W[i]),each=2^(i-1))[1:742]
  
}
test<-unlist(data_wvlt@W)

baumWelch(HMM, observations, maxIterations=1000, delta=1E-9, pseudoCount=0)
