library(HMM)



T<-300;
N<-2;
K<-6;

transition=array(c(0.7,0.4,0.3,0.6),c(N,N))


start_param<-array(1/N, c(N))

emission<-array(1/(K),c(K,N))

alpha<-array(0,c(N,T))
beta<-array(0,c(N,T))
gamma<-array(0,c(N,T))
cross_state<-array(0,c(N,N,T-1))


true_transition<-array(c(0.93,0.12,0.07,0.88),c(N,N))
true_emissions<-array(c(0.17,0.17,0.17,0.17,0.17,0.17,0.1,0.11,0.1,0.11,0.1,0.48),c(K,N))

true_data<-create_observations(true_transition,true_emissions,start_state=1,N,T,K)

true_states<-unlist(true_data[1])
observations<-unlist(true_data[2])

HMM<-initHMM(c('honest','cheat'), c('1','2','3','4','5','6'), startProbs=start_param, transProbs=transition, emissionProbs=aperm(emission))

baumWelch(HMM, observations, maxIterations=1000, delta=1E-9, pseudoCount=0)


