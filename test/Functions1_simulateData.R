simulateData<-function(seed=1){
  set.seed(seed)
  n=200 #Sample size
  K=2 #Number of components (lines)
  p_s=c(0.5,0.5) #Proportions of components
  mu_s=list(c(0,-2),c(0,2)) #Mean vectors
  Sigma_s=list(rbind(c(1,0.8),c(0.8,1)),rbind(c(1,0.8),c(0.8,1))) #Covariance matrices
  z=sample(1:K,size=n,prob=p_s,replace=TRUE) #Line memberships
  data=matrix(0,nrow=n,ncol=2)
  for (i in 1:K){
    idx=which(z==i)
    data[idx,]=mvtnorm::rmvnorm(n=length(idx),mean=mu_s[[i]],sigma=Sigma_s[[i]])
  }

  return(cbind(data,z))
}

simulateData_eQTL<-function(seed=1){
  # seed<-1

  set.seed(seed)
  n=200 #Sample size
  K=2 #Number of components (lines)
  p_s=c(0.5,0.5) #Proportions of components

  x<-c(rep(0,100),rep(1,60),rep(2,40))
  z=sample(1:K,size=n,prob=p_s,replace=TRUE) #Line memberships
  y<-rep(NA,n)
  toReturn<-as.data.frame(cbind(x,y,z))

  indices<-which(z==1)
  toReturn[indices,2]<-3*toReturn[indices,1]+1.5+rnorm(n=length(indices))

  indices<-which(z==2)
  toReturn[indices,2]=-2*toReturn[indices,1]-1+rnorm(n=length(indices))

  # plot(toReturn$x,toReturn$y,col=toReturn$z)

  return(toReturn)
}








