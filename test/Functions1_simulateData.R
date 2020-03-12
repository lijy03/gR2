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
