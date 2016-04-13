

T<-100;
N<-2;
K<-3;

transition=array(c(0.7,0.4,0.3,0.6),c(N,N))



start_param<-array(1/N, c(N))

emission<-array(c(rep(1/(K),K),0.1,0.1,0.8),c(K,N))
emission[K,N]

alpha<-array(0,c(N,T))
beta<-array(0,c(N,T))
gamma<-array(0,c(N,T))
cross_state<-array(0,c(N,N,T-1))


true_transition<-array(c(0.9,0.1,0.1,0.9),c(N,N))
true_emissions<-array(c(0.3,0.3,0.4,0,0,1),c(K,N))

true_data<-create_observations(true_transition,true_emissions,start_state=1,N,T,K)

true_states<-unlist(true_data[1])
observations<-unlist(true_data[2])

diff=1
q=0
while(q<10 && diff>1E-9){
iteration<-ForwardBackward(start_param,transition,emission,observations,N,T)
alpha<-as.array(iteration$alpha)
beta<-as.array(iteration$beta)
cn<-unlist(iteration$cn)

results<-CalcVals(alpha,beta,cn,transition,emission,observations,N,T)
gamma<-as.array(results$gamma)
cross_state<-as.array(results$cross_state)


update <- M_step(gamma, cross_state,observations,K,N,T,q)
emission<-as.array(update$emission)
transition_new<-as.array(update$transition)
start_param<-as.array(update$start_param)

q=q+1
diff=sum((transition_new-transition)^2)+1
transition=transition_new
}
transition
emission


create_observations<-function(true_transition,true_emissions,start_state,N,T,K){
  
  observations<-array(0,c(T))
  
  state<-array(0,c(T))
  state[1]=start_state
  observations[1]=sample(1:K, size=1, replace=TRUE, prob=true_emissions[,state[1]])
  
  for(i in 2:T)
  {
   state[i]=sample(1:N, size=1, replace=TRUE, prob=true_transition[,state[i-1]])
   observations[i]=sample(1:K, size=1, replace=TRUE, prob=true_emissions[,state[i]])
   
  }
  
  return(list(state,observations))
  
  
}

ForwardBackward<-function(start_param,transition,emission,observations,N,T){
  
  alpha<-array(0,c(N,T))
  beta<-array(0,c(N,T))
  cn<-array(0,c(T))
  
    alpha[,1]=prod(start_param,emission[observations[1],])
    cn[1]=sum(alpha[,1])
    alpha[,1]=alpha[,1]/cn[1]
    for(i in 2:T)
    {
      alpha[,i]=(emission[observations[i],]*(alpha[,i-1]%*%transition))
      cn[i]=sum(alpha[,i])
      alpha[,i]=alpha[,i]/cn[i]
    }
  
    beta[,T]=1
    
    for(t in (T-1):1)
    {
      beta[,t]=(emission[observations[t+1],]*beta[,t+1])/cn[t+1]
      beta[,t]=transition%*%beta[,t]
    }
    
    return(list(alpha=alpha,beta=beta,cn=cn))
}

CalcVals<-function(alpha,beta,cn,transition,emission,observations,N,T){
  
  gamma<-array(0,c(N,T))
  gamma<-alpha*beta
  
  cross_state<-array(0,c(N,N,T-1))

  for(t in 1:T-1){
  cross_state[,,t]=transition*alpha[,t]*cn[t]
  cross_state[,,t]=t(t(cross_state[,,t])*(beta[,t+1]*emission[observations[t+1],]))
  }
  
 return(list(gamma=gamma,cross_state=cross_state))
}


M_step <- function(gamma, cross_state,observations,K,N,T,itcounter){
 
  emission<-array(0, c(K,N))
  transition=array(0, c(N,N));
  
  
  start_param<-gamma[,1]
  
  transition=apply(cross_state, c(1,2), sum)
  temp_sum=apply(cross_state[,,1:T-1], c(1), sum)
  transition=sweep(transition,1,temp_sum,'/')
  
  if(sum(transition)>2){
    print(transition)
    print(itcounter)
  }
  
  for(k in 1:K){
    ik<-which(observations==k)
      if(length(ik)>1){
       emission[k,]=apply(gamma[,ik],c(1),sum)
      } else if (length(ik)==1){
       emission[k,]=gamma[,ik]
      } else {
        emission[k,]=0
      }
  }
  emission_sum=apply(gamma[,1:T], c(1), sum)
  emission=sweep(emission,2,emission_sum,'/')
  
  return(list(emission=emission,transition=transition,start_param=start_param))
}



for(t in 1:T-1){
  cross_state[,,t]=alpha[,t]%*%sweep(transition,1,alpha[,t],'*')
  cross_state[,,t]=sweep(cross_state[,,t],2,beta[,t+1]*emission[observations[t+1],],'*')
  cross_state[,,t]=cross_state[,,t]*cn[t]
}
